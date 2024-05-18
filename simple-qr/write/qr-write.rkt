#lang racket

(provide (contract-out
          [qr-write (->* (string? path-string?)
                         (
                          #:mode string?
                          #:error_level string?
                          #:module_width natural?
                          #:color (cons/c string? (or/c string? 'transparent))
                          #:output_type (or/c 'png 'svg)
                         )
                         any)]
          ))

(require racket/runtime-path
         "../share/qr.rkt"
         "../share/draw/matrix.rkt"
         "../share/draw/draw.rkt"
         "../share/lib.rkt"
         "../write/finder-pattern.rkt"
         "../write/separator.rkt"
         "../write/timing-pattern.rkt"
         "../write/alignment-pattern.rkt"
         "../write/dark-module.rkt"
         "../write/format-information.rkt"
         "../write/version-information.rkt"
         "../write/data-encoding.rkt"
         "../write/bits-width.rkt"
         "../write/data-group.rkt"
         "../write/ec-count.rkt"
         "../write/remainder-bits.rkt"
         "../write/fill-data.rkt"
         "../write/mask-data.rkt"
         "../write/error-level.rkt"
         racket/runtime-path
         reed-solomon)

(define (qr-write data file_name
                  #:mode [mode 'B]
                  #:error_level [error_level 'H]
                  #:module_width [module_width 5]
                  #:color [color '("black" . "white")]
                  #:output_type [output_type 'png]
                  )

  (let ([qr (new-qr data module_width mode error_level (car color) (cdr color))])
    (draw-finder-pattern qr)

    (draw-separator qr)

    (draw-timing-pattern qr)

    (draw-alignment-pattern qr)

    (draw-dark-module qr)

    (draw-format-information "101010101010101" qr)

    (draw-version-information qr)

    (let ([s1_data_bits #f]
          [s2_character_count #f]
          [s2_1_count_bit_width #f]
          [s3_character_count_indicator #f]
          [s4_mode_indicator #f]
          [s5_header_added_bits #f]
          [s6_capacity_char_count #f]
          [s7_capacity_bits_width #f]
          [s8_terminator_appended_bits #f]
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
          [s22_trace_list #f])

      ;; data to bits
      (cond
       [(eq? mode 'A)
        (set! s1_data_bits (encode-a data))]
       [(eq? mode 'B)
        (set! s1_data_bits (encode-b data))]
       [(eq? mode 'N)
        (set! s1_data_bits (encode-n data))])

      ;; add mode and count indicator
      (set! s2_character_count (string-length data))

      (set! s2_1_count_bit_width (get-character-bit-width (QR-version qr) (QR-mode qr)))

      (set! s3_character_count_indicator
            (~r s2_character_count #:base 2 #:min-width s2_1_count_bit_width #:pad-string "0"))

      (set! s4_mode_indicator
            (cond
             [(eq? (QR-mode qr) 'N) "0001"]
             [(eq? (QR-mode qr) 'A) "0010"]
             [(eq? (QR-mode qr) 'B) "0100"]
             [(eq? (QR-mode qr) 'K) "1000"]))

      (set! s5_header_added_bits (string-append s4_mode_indicator s3_character_count_indicator s1_data_bits))

      ;; add terminator
      (set! s6_capacity_char_count (get-bits-width (QR-version qr) (QR-error_level qr)))

      (set! s7_capacity_bits_width (* 8 s6_capacity_char_count))

      (set! s8_terminator_appended_bits (add-terminator s5_header_added_bits s7_capacity_bits_width))

      ;; add multiple eight
      (set! s9_multiple8_bits (add-multi-eight s8_terminator_appended_bits))

      ;; repeat padding
      (let ([repeat_str "1110110000010001"])
        (set! s10_repeat_pad_bits (repeat-right-pad-string s9_multiple8_bits s7_capacity_bits_width "1110110000010001")))

      ;; split to groups

      ;; to decimal list
      (set! s11_decimal_list (split-bit-string-to-decimal s10_repeat_pad_bits))

      ;; group data
      (set! s12_split_contract (get-group-width (QR-version qr) (QR-error_level qr)))

      ;; split decimal list on contract
      (set! s13_origin_data_group (split-decimal-list-on-contract s11_decimal_list s12_split_contract))

      ;; calculate error code
      (set! s14_ec_count (get-ec-count (QR-version qr) (QR-error_level qr)))

      (set! s16_error_code_group
            (list
             (map
              (lambda (block_list)
                (list block_list
                      (rs-encode block_list s14_ec_count)))
              (first s13_origin_data_group))

             (map
              (lambda (block_list)
                (list block_list
                      (rs-encode block_list s14_ec_count)))
              (second s13_origin_data_group))))

      ;; interleave data group
      (set! s17_interleave_data_group (interleave-data-group s16_error_code_group))

      (set! s18_interleave_data_bits (decimal-list-to-string s17_interleave_data_group))

      ;; padded remainder bits
      (set! s19_remainder_bits_width (get-remainder-bits (QR-version qr)))

      (set! s20_padded_remainder_bits
            (~a s18_interleave_data_bits #:min-width (+ (string-length s18_interleave_data_bits) s19_remainder_bits_width) #:right-pad-string "0"))

      ;; fill data bits, skip reserved points
      (set! s22_trace_list (get-data-socket-list (QR-modules qr) #:skip_points_hash (QR-point_type_map qr)))

      (draw-data s20_padded_remainder_bits s22_trace_list qr)

      ;; mask data
      (let* ([format_str #f]
             [data_list #f]
             [mask_list #f]
             [condition_list #f]
             [penalty_list #f]
             [min_penalty #f]
             [mask_index #f])

        (set! data_list
              (let loop ([loop_trace_list s22_trace_list]
                         [loop_data_list (map (lambda (bit_char) (- (char->integer bit_char) 48)) (string->list s20_padded_remainder_bits))]
                         [result_list '()])
                (if (not (null? loop_trace_list))
                    (loop
                     (cdr loop_trace_list)
                     (cdr loop_data_list)
                     (cons (cons (car loop_trace_list) (car loop_data_list)) result_list))
                    (reverse result_list))))

        (set! mask_list (map
                         (lambda (mask_number)
                           (let ([mask_points_map (hash-copy (QR-point_val_map qr))])
                             (for-each
                              (lambda (item)
                                (hash-set! mask_points_map (car item) (cdr item)))
                              (mask-func data_list mask_number))
                             mask_points_map))
                         '(0 1 2 3 4 5 6 7)))

        (set! condition_list (map
                              (lambda (mask_points_map)
                                (list
                                 (mask-on-condition1 (QR-modules qr) mask_points_map)
                                 (mask-on-condition2 mask_points_map)
                                 (mask-on-condition3 (QR-modules qr) mask_points_map)
                                 (mask-on-condition4 (QR-modules qr) mask_points_map)))
                              mask_list))

        (set! penalty_list (map (lambda (score_list) (foldr + 0 score_list)) condition_list))

        (set! min_penalty (apply min penalty_list))

        (set! mask_index (index-of penalty_list min_penalty))

        ;; draw selected mask
        (set-QR-point_val_map! qr (list-ref mask_list mask_index))
        (set! format_str (hash-ref (get-error-code-hash) (format "~a-~a" error_level mask_index)))

        (draw-format-information format_str qr)))

    (fill-type-points 'all (cons (QR-one_color qr) (QR-zero_color qr)) qr)

    (draw (QR-matrix qr) file_name output_type)))
