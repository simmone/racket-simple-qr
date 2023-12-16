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
(require "../share/error-level.rkt")
(require "../share/character-bit-width.rkt")
(require "../share/code-info/code-info-func.rkt")

(require reed-solomon)

(define (qr-read pic_path)

  (with-handlers 
   ([exn:fail?
     (lambda (v)
       "")])

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

     (set! step1_points_list (pic->points pic_path))

     (set! image_height (length step1_points_list))

     (set! image_width (length (car step1_points_list)))

     (set! step2_threshold (find-threshold step1_points_list))

     (set! step3_bw_points (points->bw step1_points_list step2_threshold))

     (set! step4_pattern_center_points (find-pattern-center-points step3_bw_points))

     (when step4_pattern_center_points
       (let ([module_width (car step4_pattern_center_points)]
             [center_points (cdr step4_pattern_center_points)])

         (set! step5_rotate_ratio (calculate-rotate-ratio
                                   (first center_points)
                                   (second center_points)
                                   (point-distance (first center_points) (second center_points))))

         (set! step6_rotated_points
               (rotate-and-cut-bmp
                step3_bw_points
                step5_rotate_ratio
                (first center_points)
                (point-distance (first center_points) (second center_points))
                module_width))

         (set! step7_trimed_points (trim-matrix step6_rotated_points))

         (set! step8_squashed_points (squash-matrix step7_trimed_points module_width))

         (set! step9_end_points (trim-matrix (trim-tail step8_squashed_points)))

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

           (if (or (not (exact-nonnegative-integer? version)) (> version 40))
               ""
               (begin
                 (exclude-finder-pattern modules exclude_points_map)

                 (exclude-separator modules exclude_points_map)

                 (exclude-timing-pattern modules exclude_points_map timing_points_map)

                 (exclude-alignment-pattern version exclude_points_map timing_points_map)

                 (exclude-format-information modules exclude_points_map)

                 (exclude-version version modules exclude_points_map)

                 (exclude-dark-module version exclude_points_map)

                 (let* (
                        [trace_list (get-data-socket-list modules #:skip_points_hash exclude_points_map)]
                        [ref_data_list (get-points init_matrix trace_list)]

                        [group_width (get-group-width version error_level)]
                        [data_count_list (defines->count-list group_width)]
                        [data_total_count (apply + data_count_list)]
                        [ec_count (get-ec-count version error_level)]
                        [ec_count_list (map (lambda (group_data_count) ec_count) data_count_list)]
                        [ec_total_count (apply + ec_count_list)]

                        [mask-proc (get-mask-proc (string->number mask_pattern))]
                        [unmasked (get-unmask-points init_matrix trace_list mask-proc)]
                        [unmasked_data (car unmasked)]
                        [mask_list (cdr unmasked)]

                        [unmasked_data_bits #f]

                        [bit8_list #f]
                        [data_bit8_list #f]
                        [data_sequence #f]
                        [ec_bit8_list #f]
                        [ec_sequence #f]

                        [un_interleave_data_bits #f]
                        [un_interleave_ec_bits #f]
                        
                        [data_byte_list #f]
                        [ec_byte_list #f]

                        [data_bits #f]

                        [raw_data_groups #f]
                        [recovered_data_groups #f]
                        )

                   (set! unmasked_data_bits (foldr (lambda (a b) (string-append a b)) ""
                                                   (map (lambda (item) (number->string item)) unmasked_data)))

                   (set! bit8_list (split-data->groups unmasked_data_bits (+ data_total_count ec_total_count)))

                   (set! data_bit8_list (take bit8_list data_total_count))

                   (set! data_sequence (sequence_list->sequence (count_list->sequence_list data_count_list)))

                   (set! ec_bit8_list (take-right bit8_list ec_total_count))

                   (set! ec_sequence (sequence_list->sequence (count_list->sequence_list ec_count_list)))

                   (set! un_interleave_data_bits
                         (foldr
                          (lambda (a b)
                            (string-append a b))
                          ""
                          (map
                           (lambda (rec)
                             (car rec))
                           (sort
                            (combine-data-sequence
                             data_bit8_list
                             data_sequence)
                            <
                            #:key cdr))))

                   (set! un_interleave_ec_bits
                         (foldr
                          (lambda (a b)
                            (string-append a b))
                          ""
                          (map
                           (lambda (rec)
                             (car rec))
                           (sort
                            (combine-data-sequence
                             ec_bit8_list
                             ec_sequence)
                            <
                            #:key cdr))))

                   (set! data_byte_list
                         (map
                          (lambda (bit8)
                            (string->number bit8 2))
                          (split-data->groups un_interleave_data_bits data_total_count)))

                   (set! ec_byte_list
                         (map
                          (lambda (bit8)
                            (string->number bit8 2))
                          (split-data->groups un_interleave_ec_bits ec_total_count)))

                   (set! raw_data_groups 
                         (let loop (
                                    [loop_data_counts data_count_list]
                                    [loop_data_list data_byte_list]
                                    [loop_ec_counts ec_count_list]
                                    [loop_ec_list ec_byte_list]
                                    [result_list '()]
                                    )
                           (if (not (null? loop_data_counts))
                               (loop
                                (cdr loop_data_counts)
                                (drop loop_data_list (car loop_data_counts))
                                (cdr loop_ec_counts)
                                (drop loop_ec_list (car loop_ec_counts))
                                (cons
                                 `(
                                   ,@(take loop_data_list (car loop_data_counts))
                                   ,@(take loop_ec_list (car loop_ec_counts)))
                                 result_list))
                               (reverse result_list))))
                   
                   (set! recovered_data_groups 
                         (map
                          (lambda (bt_group)
                            (rs-decode bt_group ec_count))
                          raw_data_groups))
                   
                   (set! data_bits
                         (foldr (lambda (a b) (string-append a b)) ""
                                (map
                                 (lambda (bt)
                                   (~r #:base 2 #:min-width 8 #:pad-string "0" bt))
                                 (let loop ([loop_count data_count_list]
                                            [loop_groups recovered_data_groups]
                                            [result '()])
                                   (if (not (null? loop_count))
                                       (loop
                                        (cdr loop_count)
                                        (cdr loop_groups)
                                        (cons (take (car loop_groups) (car loop_count)) result))
                                       (flatten (reverse result)))))))

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

                     (bytes->string/utf-8 bytes_list)
                     ))))))))))
