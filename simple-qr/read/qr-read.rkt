#lang racket

(provide (contract-out
          [qr-read (-> path-string? string?)]
          ))

(require "lib/lib.rkt")
(require "../share/func.rkt")
(require "../share/format-information.rkt")
(require "../share/mask-data.rkt")
(require "../share/fill-data.rkt")
(require "../share/data-group.rkt")
(require "../share/data-encoding.rkt")

(define (qr-read pic_path)
  (let ([data_str ""])
    (with-handlers
     ([exn:fail?
       (lambda (e)
         (void))])
     (let* ([step1_points_list #f]
            [original_height #f]
            [original_width #f]
            [step2_threshold #f]
            [step3_bw_points #f]
            [step4_pattern_center_points #f]
            [step5_rotate_ratio #f]
            [step6_rotated_points #f]
            [step7_trimed_points #f]
            [step8_squashed_points #f]
            [step9_end_points #f]
            )

       (set! step1_points_list (pic->points pic_path))
       (set! original_width (length step1_points_list))
       (set! original_height (length (car step1_points_list)))
       (appTrace *TRACE_INFO* (lambda () (printf "step1:convert pic file to pixel points[~aX~a]\n" original_width original_height)))
       
       (set! step2_threshold (find-threshold step1_points_list))
       (appTrace *TRACE_INFO* (lambda () (printf "step2:find threshold is ~a\n" step2_threshold)))

       (set! step3_bw_points (points->bw step1_points_list step2_threshold))
       (appTrace *TRACE_INFO* (lambda () (printf "step3:use threshold convert pixel to points 0 or 1\n")))
       (appTrace *TRACE_DEBUG* (lambda () (points->pic step3_bw_points "step3_bw.png" (make-hash))))
       
       (set! step4_pattern_center_points (find-pattern-center-points step3_bw_points))
       (appTrace *TRACE_INFO* (lambda () (printf "step4 pattern center points:~a\n" step4_pattern_center_points)))

       (when step4_pattern_center_points
             (let ([pixel_map (make-hash)]
                   [module_width (car step4_pattern_center_points)]
                   [center_points (cdr step4_pattern_center_points)])
               (hash-set! pixel_map (first center_points) '(0 0 255 255))
               (hash-set! pixel_map (second center_points) '(0 255 0 255))
               (hash-set! pixel_map (third center_points) '(255 0 0 255))

               (appTrace *TRACE_DEBUG* (lambda () (points->pic step3_bw_points "step4_pattern_center.png" pixel_map)))
               
               (set! step5_rotate_ratio (calculate-rotate-ratio 
                                         (first center_points) 
                                         (second center_points) 
                                         (point-distance (first center_points) (second center_points))))
               (appTrace *TRACE_INFO* (lambda () (printf "step5 rotate ratio:~a\n" step5_rotate_ratio)))

               (set! step6_rotated_points
                     (rotate-and-cut-bmp
                      step3_bw_points
                      step5_rotate_ratio 
                      (first center_points) 
                      (point-distance (first center_points) (second center_points))
                      module_width))

               (appTrace *TRACE_DEBUG* (lambda () (points->pic step6_rotated_points "step6_rotated.png" (make-hash))))
               (appTrace *TRACE_INFO* (lambda () (printf "step6 rotate and cut complete.\n")))
               
               (set! step7_trimed_points (trim-matrix step6_rotated_points))
               (appTrace *TRACE_DEBUG* (lambda () (points->pic step7_trimed_points "step7_trimed.png" (make-hash))))
               
               (set! step8_squashed_points (squash-matrix step7_trimed_points module_width))
               (appTrace *TRACE_DEBUG* 
                         (lambda ()
                           (points->pic step8_squashed_points "step8_squashed.png" (make-hash))
                           (printf "step8 points:\n")
                           (print-matrix step8_squashed_points)))

               (set! step9_end_points (trim-matrix (trim-tail step8_squashed_points)))
               (appTrace *TRACE_DEBUG*
                         (lambda ()
                           (points->pic step9_end_points "step9_end.png" (make-hash))
                           (printf "step9 points:\n")
                           (print-matrix step9_end_points)))
               
               (let* ([init_matrix step9_end_points]
                      [width (length (car init_matrix))]
                      [version #f]
                      [format_code_error_hash (get-code-error-hash)]
                      [format_information #f]
                      [error_level #f]
                      [mask_pattern #f]
                      [mask-proc #f]
                      [exclude_points_map (make-hash)]
                      [timing_points_map (make-hash)]
                      [new_exclude_points_map (make-hash)])

                 (set! version (add1 (/ (- (length (car init_matrix)) 21) 4)))


                 (set! format_information (hash-ref format_code_error_hash 
                                                    (foldr (lambda (a b) 
                                                             (string-append a b)) "" 
                                                             (map (lambda (item) (number->string item))
                                                                  (reverse
                                                                   (get-points init_matrix 
                                                                               (transform-points-list (first (get-format-information)) '(1 . 1))))))))
                 (set! error_level (substring format_information 0 1))
                 (set! mask_pattern (substring format_information 2 3))
                 (appTrace *TRACE_INFO* 
                           (lambda () 
                             (printf "step9:width:~a, version:~a, format_information;~a, error_level:~a, mask_pattern:~a\n" 
                                     width version format_information error_level mask_pattern)))
                 (set! mask-proc (get-mask-proc (string->number mask_pattern)))

                 (if (or (not (exact-nonnegative-integer? version)) (> version 40))
                     ""
                     (begin
                       (exclude-finder-pattern width exclude_points_map)
                       (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step91_exclude_finder_pattern.png" exclude_points_map)))
                       (exclude-separator width exclude_points_map)
                       (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step92_exclude_separator.png" exclude_points_map)))
                       (exclude-timing-pattern width exclude_points_map timing_points_map)
                       (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step93_exclude_timing_pattern.png" exclude_points_map)))
                       (exclude-alignment-pattern version exclude_points_map timing_points_map)
                       (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step94_exclude_alignment_pattern.png" exclude_points_map)))
                       (exclude-format-information width exclude_points_map)
                       (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step95_exclude_format_information.png" exclude_points_map)))
                       (exclude-version version width exclude_points_map)
                       (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step96_exclude_version.png" exclude_points_map)))
                       (exclude-dark-module version exclude_points_map)
                       (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step97_exclude_dark_module.png" exclude_points_map)))
                       
                       (hash-for-each
                        exclude_points_map
                        (lambda (point val)
                          (hash-set! new_exclude_points_map (cons (add1 (car point)) (add1 (cdr point))) '(0 0 255 255))))

                       (let* ([trace_list (get-data-socket-list width #:skip_points_hash new_exclude_points_map)]
                              [data_list (get-points init_matrix trace_list)]
                              [unmask_data_bits #f]
                              [data_bits #f]
                              [mode #f])

                         (appTrace *TRACE_DEBUG* 
                                   (lambda () 
                                     (printf "mask data:~a\n" 
                                             (foldr (lambda (a b) (string-append a b)) "" 
                                                    (map (lambda (item) (number->string item)) data_list)))))
                         (let* ([unmask_data (get-unmask-points init_matrix trace_list mask-proc)]
                                [data (car unmask_data)]
                                [mask_list (cdr unmask_data)])
                           (appTrace *TRACE_DEBUG* 
                                     (lambda () (printf "mask list:~a\n" 
                                                        (foldr (lambda (a b) (string-append a b)) "" 
                                                               (map (lambda (item) (number->string item)) mask_list)))))
                           (set! unmask_data_bits (foldr (lambda (a b) (string-append a b)) "" 
                                                         (map (lambda (item) (number->string item)) data)))
                           (appTrace *TRACE_DEBUG* (lambda () (printf "unmask data:~a, [~a]\n" unmask_data_bits (string-length unmask_data_bits))))
                           (appTrace *TRACE_DEBUG* (lambda () (printf "group width:~a\n" (get-group-width version error_level))))
                           (set! data_bits (rearrange-data unmask_data_bits (defines->count-list (get-group-width version error_level))))

                           (appTrace *TRACE_DEBUG* (lambda () (printf "data:~a\n" data_bits)))
                           
                           (let ([mode (get-indicator-mode (substring data_bits 0 4))]
                                 [data_count (string->number (substring data_bits 4 12) 2)])
                             (appTrace *TRACE_INFO* (lambda () (printf "head :mode:~a, data_count:~a\n" mode data_count)))
                             
                             (set! data_str
                                   (bytes->string/utf-8 
                                    (list->bytes
                                     (map
                                      (lambda (rec)
                                        (string->number rec 2))
                                      (let loop ([loop_count data_count]
                                                 [loop_str (substring data_bits 12)]
                                                 [result_list '()])
                                        (if (> loop_count 0)
                                            (loop
                                             (sub1 loop_count)
                                             (substring loop_str 8)
                                             (cons (substring loop_str 0 8) result_list))
                                            (reverse result_list)))))))
                             (appTrace *TRACE_INFO* (lambda () (printf "data:~a\n" data_str)))))
                         ))))))))
       data_str))
