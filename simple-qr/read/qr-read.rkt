#lang racket

(provide (contract-out
          [qr-read (->* (path-string?) (#:express? boolean? #:express_path path-string?) string?)]
          ))

(require "lib/lib.rkt")
(require "../share/func.rkt")
(require "../share/format-information.rkt")
(require "../share/mask-data.rkt")
(require "../share/fill-data.rkt")
(require "../share/data-group.rkt")
(require "../share/data-encoding.rkt")
(require "../share/error-level.rkt")
(require "../share/character-bit-width.rkt")
(require "lib/express/express.rkt")

(define (qr-read pic_path
                 #:express? [express? #f]
                 #:express_path [express_path (string->path (string-append (path->string pic_path) ".read.express"))])

  (with-handlers 
   ([(lambda (v) #t) (lambda (v) "")])
   (when express?
         (delete-directory/files #:must-exist? #f express_path)
         (make-directory* express_path)
         (let ([input_img (build-path express_path "input.img")])
           (copy-file pic_path input_img)))

   (express express? (lambda () (write-report-header express_path)))

   (let ([step1_points_list #f]
         [image_height #f]
         [image_width #f]
         [step2_threshold #f]
         [step3_bw_points #f]
         [step4_pattern_center_points #f]
         [step5_rotate_ratio #f]
         [step6_rotated_points #f]
         [step7_trimed_points #f]
         [step8_squashed_points #f]
         [step9_end_points #f])

     (express express? (lambda () (write-report-input express_path)))

     (set! step1_points_list (pic->points pic_path))

     (set! image_height (length step1_points_list))

     (set! image_width (length (car step1_points_list)))

     (express express? (lambda () (write-report-origin-bits image_height image_width step1_points_list express_path)))

     (set! step2_threshold (find-threshold step1_points_list))

     (set! step3_bw_points (points->bw step1_points_list step2_threshold))

     (express express? (lambda ()
                         (write-report-bw-bits step2_threshold image_width step3_bw_points express_path)))

     (set! step4_pattern_center_points (find-pattern-center-points step3_bw_points))

     (when step4_pattern_center_points
           (let ([module_width (car step4_pattern_center_points)]
                 [center_points (cdr step4_pattern_center_points)])

             (express express?
                      (lambda ()
                        (write-report-finder-pattern-center-points
                         module_width
                         center_points
                         image_width
                         step3_bw_points express_path)))

             (set! step5_rotate_ratio (calculate-rotate-ratio
                                       (first center_points)
                                       (second center_points)
                                       (point-distance (first center_points) (second center_points))))

             (express express?
                      (lambda ()
                        (write-report-rotate-ratio step5_rotate_ratio express_path)))

             (set! step6_rotated_points
                   (rotate-and-cut-bmp
                    step3_bw_points
                    step5_rotate_ratio
                    (first center_points)
                    (point-distance (first center_points) (second center_points))
                    module_width))
             (express express?
                      (lambda ()
                        (write-report-rotated-bits image_width step6_rotated_points express_path)))

             (set! step7_trimed_points (trim-matrix step6_rotated_points))

             (express express?
                      (lambda ()
                        (write-report-trimed-bits image_width step7_trimed_points express_path)))

             (set! step8_squashed_points (squash-matrix step7_trimed_points module_width))

             (set! step9_end_points (trim-matrix (trim-tail step8_squashed_points)))

             (express express?
                      (lambda ()
                        (write-report-final-bits module_width step9_end_points express_path)))

             (let* ([init_matrix step9_end_points]
                    [modules (length (car init_matrix))]
                    [version #f]
                    [format_string #f]
                    [format_code_error_hash (get-code-error-hash)]
                    [format_information #f]
                    [error_level #f]
                    [mask_pattern #f]
                    [points_map (points->points_map step9_end_points)]
                    [exclude_points_map (make-hash)]
                    [timing_points_map (make-hash)])

               (set! version (add1 (/ (- (length (car init_matrix)) 21) 4)))
               
               (set! format_string
                     (foldr (lambda (a b)
                              (string-append a b)) ""
                              (map (lambda (item) (number->string item))
                                   (reverse
                                    (get-points init_matrix
                                                (transform-points-list (first (get-format-information)) '(1 . 1)))))))

               (set! format_information (hash-ref format_code_error_hash format_string))

               (set! error_level (substring format_information 0 1))

               (set! mask_pattern (substring format_information 2 3))

               (express express?
                        (lambda ()
                          (write-report-basic-information 
                           version 
                           format_string 
                           format_information 
                           error_level 
                           mask_pattern 
                           express_path)))

               (if (or (not (exact-nonnegative-integer? version)) (> version 40))
                   ""
                   (begin
                     (exclude-finder-pattern modules exclude_points_map)
                     (express express?
                              (lambda ()
                                (write-report-exclude-finder-pattern
                                 modules
                                 points_map
                                 exclude_points_map
                                 express_path)))

                     (exclude-separator modules exclude_points_map)
                     (express express?
                              (lambda ()
                                (write-report-exclude-separator
                                 modules
                                 points_map
                                 exclude_points_map
                                 express_path)))

                     (exclude-timing-pattern modules exclude_points_map timing_points_map)
                     (express express?
                              (lambda ()
                                (write-report-exclude-timing-pattern
                                 modules
                                 points_map
                                 exclude_points_map
                                 express_path)))

                     (exclude-alignment-pattern version exclude_points_map timing_points_map)
                     (express express?
                              (lambda ()
                                (write-report-exclude-alignment-pattern
                                 modules
                                 points_map
                                 exclude_points_map
                                 express_path)))

                     (exclude-format-information modules exclude_points_map)
                     (express express?
                              (lambda ()
                                (write-report-exclude-format-information
                                 modules
                                 points_map
                                 exclude_points_map
                                 express_path)))

                     (exclude-version version modules exclude_points_map)
                     (express express?
                              (lambda ()
                                (write-report-exclude-version-information
                                 modules
                                 points_map
                                 exclude_points_map
                                 express_path)))

                     (exclude-dark-module version exclude_points_map)
                     (express express?
                              (lambda ()
                                (write-report-exclude-dark-module
                                 modules
                                 points_map
                                 exclude_points_map
                                 express_path)))

                     (let* ([trace_list (get-data-socket-list modules #:skip_points_hash exclude_points_map)]
                            [data_list (get-points init_matrix trace_list)]
                            [unmasked_data_bits #f]
                            [data_bits #f]
                            [mode #f]
                            [mask-proc (get-mask-proc (string->number mask_pattern))]
                            [unmasked (get-unmask-points init_matrix trace_list mask-proc)]
                            [unmasked_data (car unmasked)]
                            [mask_list (cdr unmasked)]
                            [group_width #f]
                            [count_list #f]
                            [data_groups #f]
                            [sequence #f])

                       (set! unmasked_data_bits (foldr (lambda (a b) (string-append a b)) ""
                                                       (map (lambda (item) (number->string item)) unmasked_data)))

                       (express express?
                                (lambda ()
                                  (write-report-unmask-data
                                   data_list
                                   mask_list
                                   unmasked_data_bits
                                   express_path)))

                       (set! group_width (get-group-width version error_level))
                       
                       (set! count_list (defines->count-list group_width))

                       (set! data_groups (split-data->groups unmasked_data_bits (apply + count_list)))
                       
                       (set! sequence (sequence_list->sequence (count_list->sequence_list count_list)))

                       (set! data_bits 
                             (foldr
                              (lambda (a b)
                                (string-append a b))
                              ""
                              (map
                               (lambda (rec)
                                 (car rec))
                               (sort
                                (combine-data-sequence
                                 data_groups
                                 sequence)
                                <
                                #:key cdr))))

                       (express express?
                                (lambda ()
                                  (write-report-ungroup-data
                                   group_width
                                   count_list
                                   data_groups
                                   sequence
                                   data_bits
                                   express_path)))

                       (let* ([mode (get-indicator-mode (substring data_bits 0 4))]
                              [capacity_bit_width (get-character-bit-width version mode)]
                              [indicator_index (+ 4 capacity_bit_width)]
                              [data_count (string->number (substring data_bits 4 indicator_index) 2)]
                              [bit8_list #f]
                              [bytes_list #f])
                         
                         (set! bit8_list (take (split-string (substring data_bits indicator_index) 8) data_count))

                         (set! bytes_list
                               (list->bytes
                                (map
                                 (lambda (rec)
                                   (string->number rec 2))
                                 bit8_list)))

                         (express express?
                                  (lambda ()
                                    (write-report-decoded-data
                                     mode
                                     data_count
                                     bytes_list
                                     bit8_list
                                     express_path)))

                         (express express?
                                  (lambda ()
                                    (write-report-final-string
                                     (bytes->string/utf-8 bytes_list)
                                     express_path)))

                         (bytes->string/utf-8 bytes_list)
                         ))))))))))
