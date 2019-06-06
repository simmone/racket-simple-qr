#lang racket

(provide (contract-out
          [qr-write (->* (string? path-string?) 
                         (
                          #:mode string?
                          #:error_level string?
                          #:module_width exact-nonnegative-integer?
                          #:express? boolean?
                          #:express_path path-string?
                          #:output_type (or/c 'png 'svg)
                          )
                         any)]
          ))

(require "lib/finder-pattern/finder-pattern.rkt")
(require "lib/separator/separator.rkt")
(require "lib/format-information/format-information.rkt")
(require "lib/version-information/version-information.rkt")
(require "lib/timing-pattern/timing-pattern.rkt")
(require "lib/alignment-pattern/alignment-pattern.rkt")
(require "lib/dark-module/dark-module.rkt")
(require "lib/data-encoding/data-encoding.rkt")
(require "lib/fill-data/fill-data.rkt")
(require "lib/mask-data/mask-data.rkt")
(require "lib/express/express.rkt")
(require "lib/func/func.rkt")
(require "lib/func/remainder-bits/remainder-bits-func.rkt")
(require "../share/code-info/code-info-func.rkt")
(require "../share/fill-data.rkt")
(require "../share/error-level.rkt")
(require "../share/data-encoding.rkt")
(require "../share/data-group.rkt")
(require "../share/version-information.rkt")
(require "../share/character-bit-width.rkt")
(require "../share/func.rkt")
(require "../share/draw/draw.rkt")

(require reed-solomon)

(define (qr-write data file_name
                  #:mode [mode "B"]
                  #:error_level [error_level "H"]
                  #:module_width [module_width 5]
                  #:express? [express? #f]
                  #:express_path [express_path ".write.express"]
                  #:output_type [output_type 'png]
                  )

  (when express?
        (delete-directory/files #:must-exist? #f express_path)
        (make-directory* express_path))

  (let* ([version (get-version data mode error_level)]
         [modules (version->modules version)])

    (express express? (lambda () (write-report-header express_path)))

    (express express?
             (lambda () (write-report-input data mode error_level version modules module_width express_path)))

    (express express?
             (lambda () (write-report-overview express_path)))

    (express express?
             (lambda () (write-report-start modules express_path)))

    (let* ([points_map (make-hash)]
           [type_map (make-hash)]
           [sum_count (* modules modules)])

      (draw-finder-pattern modules points_map type_map)
      (express express?
               (lambda () (write-report-finder-pattern points_map modules express_path)))

      (draw-separator modules points_map type_map)
      (express express?
               (lambda () (write-report-separator points_map modules express_path)))

      (draw-timing-pattern modules points_map type_map)
      (express express?
               (lambda () (write-report-timing-pattern points_map modules express_path)))

      (draw-alignment-pattern version points_map type_map)
      (express express?
               (lambda () (write-report-alignment-pattern points_map modules express_path)))

      ; 111100011011100 used to verify data fill
      (draw-format-information "111100011011100" modules points_map type_map)
      (express express?
               (lambda () (write-report-reserved-format-information points_map modules express_path)))

      (if (>= version 7)
          (let ([version_str (hash-ref (get-version-hash) version)])
            (draw-version-information version_str modules points_map type_map)
            (express express?
                     (lambda () (write-report-version-information version_str points_map modules express_path))))
          (express express?
                   (lambda () (write-report-version-information "" points_map modules express_path))))

      (draw-dark-module version points_map type_map)
      (express express?
               (lambda () (write-report-dark-module points_map modules express_path)))

      (let ([s1_data_bits #f]
            [s2_character_count #f]
            [s3_character_count_indicator #f]
            [s4_mode_indicator #f]
            [s5_header_added_bits #f]
            [s6_capacity_char_count #f]
            [s7_capacity_bits_width #f]
            [s8_terminator_added_bits #f]
            [s9_multiple8_bits #f]
            [s10_repeat_pad_bits #f]
            [s11_decimal_list #f]
            [s12_split_contract #f]
            [s13_origin_data_group #f]
            [s14_ec_count #f]
            [s15_origin_poly_generator #f]
            [s16_error_code_group #f]
            [s17_interleave_data_group #f]
            [s18_interleave_data_bits #f]
            [s19_remainder_bits_width #f]
            [s20_padded_remainder_bits #f]
            [s21_data_list #f]
            [s22_trace_list #f]
            )

        ;; data to bits
        (cond
         [(string=? mode "A")
          (set! s1_data_bits (encode-a data))]
         [(string=? mode "B")
          (set! s1_data_bits (encode-b data))]
         [(string=? mode "N")
          (set! s1_data_bits (encode-n data))])

        (express express?
                 (lambda () (write-report-data-bits data s1_data_bits express_path)))

        ;; add mode and count indicator
        (set! s2_character_count (string-length data))

        (set! s3_character_count_indicator (get-character-count-indicator s2_character_count version mode))

        (set! s4_mode_indicator (get-mode-indicator mode))

        (set! s5_header_added_bits (string-append s4_mode_indicator s3_character_count_indicator s1_data_bits))

        (express express?
                 (lambda ()
                   (write-report-add-indicator
                    s2_character_count
                    (get-character-bit-width version mode)
                    s3_character_count_indicator mode
                    s4_mode_indicator
                    s5_header_added_bits
                    express_path)))

        ;; add terminator
        (set! s6_capacity_char_count (get-bits-width version error_level))

        (set! s7_capacity_bits_width (* 8 s6_capacity_char_count))

        (set! s8_terminator_added_bits (add-terminator s5_header_added_bits s7_capacity_bits_width))

        (express express?
                 (lambda ()
                   (write-report-add-terminator
                    s6_capacity_char_count
                    s7_capacity_bits_width
                    s8_terminator_added_bits
                    express_path)))

        ;; add multiple eight
        (set! s9_multiple8_bits (add-multi-eight s8_terminator_added_bits))

        (express express?
                 (lambda ()
                   (write-report-add-multiple8 s9_multiple8_bits express_path)))

        ;; repeat padding
        (let ([repeat_str "1110110000010001"])
          (set! s10_repeat_pad_bits (repeat-right-pad-string s9_multiple8_bits s7_capacity_bits_width repeat_str))

          (express express?
                   (lambda ()
                     (write-report-repeat-pad s7_capacity_bits_width repeat_str s10_repeat_pad_bits express_path))))

        ;; to decimal list
        (set! s11_decimal_list (split-bit-string-to-decimal s10_repeat_pad_bits))
        (express express?
                 (lambda ()
                   (write-report-decimal-list s10_repeat_pad_bits s11_decimal_list express_path)))

        ;; group data
        (set! s12_split_contract (get-group-width version error_level))
        
        ;; split decimal list on contract
        (set! s13_origin_data_group (split-decimal-list-on-contract s11_decimal_list s12_split_contract))
        (express express?
                 (lambda ()
                   (write-report-group-decimal-list s12_split_contract s13_origin_data_group express_path)))

        ;; calculate error code
        (set! s14_ec_count (get-ec-count version error_level))
        
        (set! s16_error_code_group
              (list
               (map
                (lambda (block_list)
                  (list block_list
                        (rs-encode block_list s14_ec_count)))
                (car s13_origin_data_group))

               (map
                (lambda (block_list)
                  (list block_list
                        (rs-encode block_list s14_ec_count)))
                (cadr s13_origin_data_group))))

        (express express?
                 (lambda ()
                   (write-report-error-code s14_ec_count s16_error_code_group express_path)))

        ;; interleave data group
        (set! s17_interleave_data_group (interleave-data-group s16_error_code_group))
        
        (set! s18_interleave_data_bits (decimal-list-to-string s17_interleave_data_group))

        (express express?
                 (lambda ()
                   (write-report-interleave-data-group s17_interleave_data_group s18_interleave_data_bits express_path)))
        
        ;; padded remainder bits
        (set! s19_remainder_bits_width (get-remainder-bits version))

        (set! s20_padded_remainder_bits (~a s18_interleave_data_bits #:min-width (+ (string-length s18_interleave_data_bits) s19_remainder_bits_width) #:right-pad-string "0"))

        (set! s21_data_list (string->list s20_padded_remainder_bits))
        (express express?
                 (lambda ()
                   (write-report-append-remainder s19_remainder_bits_width s20_padded_remainder_bits express_path)))

        ;; draw data on coodinate trace
        (set! s22_trace_list (get-data-socket-list modules #:skip_points_hash points_map))
        (express express?
                 (lambda ()
                   (write-report-data-trace s22_trace_list s21_data_list points_map modules express_path)))

        (draw-data s21_data_list s22_trace_list points_map type_map)
        (express express?
                 (lambda () (write-report-fill-data points_map modules express_path)))

        ;; mask data
        (let* ([format_str #f]
               [data_list #f]
               [mask_list #f]
               [penalty_list #f]
               [min_penalty #f]
               [mask_index #f])

          (set! data_list
                (let loop ([loop_trace_list s22_trace_list]
                           [loop_data_list (map (lambda (ch) (string ch)) s21_data_list)]
                           [result_list '()])
                  (if (not (null? loop_trace_list))
                      (loop
                       (cdr loop_trace_list)
                       (cdr loop_data_list)
                       (cons (cons (car loop_trace_list) (car loop_data_list)) result_list))
                      (reverse result_list))))
          
          (set! mask_list (map
                           (lambda (mask_number)
                             (let ([new_points_map (hash-copy points_map)])
                               (for-each
                                (lambda (item)
                                  (hash-set! new_points_map (car item) (cdr item)))
                                (mask-func data_list mask_number))
                               new_points_map))
                           '(0 1 2 3 4 5 6 7)))

          (set! penalty_list (map
                              (lambda (new_points_map)
                                (+
                                 (mask-on-condition1 modules new_points_map)
                                 (mask-on-condition2 new_points_map)
                                 (mask-on-condition3 modules new_points_map)
                                 (mask-on-condition4 new_points_map)))
                              mask_list))

          (express express?
                   (lambda () (write-report-mask-list mask_list penalty_list modules express_path)))

          (set! min_penalty (apply min penalty_list))

          (set! mask_index (index-of penalty_list min_penalty))

          (set! points_map (list-ref mask_list mask_index))

          (express express?
                   (lambda () (write-report-masked min_penalty mask_index points_map modules express_path)))

          (set! format_str (hash-ref (get-error-code-hash) (string-append error_level "-" (number->string mask_index))))

          (draw-format-information format_str modules points_map type_map)

          (express express?
                   (lambda () (write-report-format-information format_str points_map modules express_path)))

          (express express?
                   (lambda () (write-report-final points_map modules express_path)))
          )

        (parameterize
         ([*output_type* output_type])
         (draw modules module_width points_map (make-hash) file_name))))))
