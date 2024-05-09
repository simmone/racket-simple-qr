#lang racket

(require racket/runtime-path
         "../share/qr.rkt"
         "../share/draw/matrix.rkt"
         "../share/draw/draw.rkt"
         "../share/lib.rkt"
         "write/get-version-express.rkt"
         "write/version-to-module-express.rkt"
         "../write/finder-pattern.rkt"
         "write/finder-pattern-express.rkt"
         "../write/separator.rkt"
         "write/separator-express.rkt"
         "../write/timing-pattern.rkt"
         "write/timing-pattern-express.rkt"
         "../write/alignment-pattern.rkt"
         "write/alignment-pattern-express.rkt"
         "../write/dark-module.rkt"
         "write/dark-module-express.rkt"
         "../write/format-information.rkt"
         "write/format-information-express.rkt"
         "../write/version-information.rkt"
         "write/version-information-express.rkt"
         "../write/data-encoding.rkt"
         "write/s10-data-encoding-express.rkt"
         "write/s11-head-bits-express.rkt"
         "../write/bits-width.rkt"
         "write/s12-terminator-express.rkt"
         "write/s13-padding-multiple8-express.rkt"
         "write/s14-repeat-pad-bits-express.rkt"
         "../write/data-group.rkt"
         "write/s15-split-to-groups-express.rkt"
         "../write/ec-count.rkt"
         "write/s16-error-correction-express.rkt"
         "write/s17-interleave-data-group-express.rkt"
         "../write/remainder-bits.rkt"
         "write/s18-add-remainder-bits-express.rkt"
         "../write/fill-data.rkt"
         "write/s19-draw-data-bits-express.rkt"
         "../write/mask-data.rkt"
         "write/s20s-mask-showcase-express.rkt"
         "write/s20-draw-mask-express.rkt"
         "../write/error-level.rkt"
         "write/s21-draw-mask-and-format-express.rkt"
         "write/s22-qr-express.rkt"
         racket/runtime-path
         reed-solomon)

(define-runtime-path content_directory (build-path "express" "content"))
(define-runtime-path index_md_file (build-path "express" "content" "_index.md"))
(define-runtime-path init_file (build-path "express" "content" "docs" "s2_module" "init.svg"))
(define-runtime-path finder_pattern_file (build-path "express" "content" "docs" "s3_finder_pattern" "finder_pattern.svg"))
(define-runtime-path separator_file (build-path "express" "content" "docs" "s4_separator" "separator.svg"))
(define-runtime-path timing_pattern_file (build-path "express" "content" "docs" "s5_timing_pattern" "timing_pattern.svg"))
(define-runtime-path alignment_pattern_file (build-path "express" "content" "docs" "s6_alignment_pattern" "alignment_pattern.svg"))
(define-runtime-path dark_module_file (build-path "express" "content" "docs" "s7_dark_module" "dark_module.svg"))
(define-runtime-path format_information_file (build-path "express" "content" "docs" "s8_format_information" "format_information.svg"))
(define-runtime-path version_information_file (build-path "express" "content" "docs" "s9_version_information" "version_information.svg"))
(define-runtime-path data_bits_file (build-path "express" "content" "docs" "s19_draw_data_bits" "data_bits.svg"))
(define-runtime-path mask_showcase_init_file (build-path "express" "content" "docs" "s20s_mask_showcase" "original.svg"))
(define-runtime-path mask_showcase_bits_0_file (build-path "express" "content" "docs" "s20s_mask_showcase" "mask0.svg"))
(define-runtime-path mask_showcase_bits_1_file (build-path "express" "content" "docs" "s20s_mask_showcase" "mask1.svg"))
(define-runtime-path mask_showcase_bits_2_file (build-path "express" "content" "docs" "s20s_mask_showcase" "mask2.svg"))
(define-runtime-path mask_showcase_bits_3_file (build-path "express" "content" "docs" "s20s_mask_showcase" "mask3.svg"))
(define-runtime-path mask_showcase_bits_4_file (build-path "express" "content" "docs" "s20s_mask_showcase" "mask4.svg"))
(define-runtime-path mask_showcase_bits_5_file (build-path "express" "content" "docs" "s20s_mask_showcase" "mask5.svg"))
(define-runtime-path mask_showcase_bits_6_file (build-path "express" "content" "docs" "s20s_mask_showcase" "mask6.svg"))
(define-runtime-path mask_showcase_bits_7_file (build-path "express" "content" "docs" "s20s_mask_showcase" "mask7.svg"))
(define-runtime-path mask_original_file (build-path "express" "content" "docs" "s20_draw_mask" "original.svg"))
(define-runtime-path mask_bits_0_file (build-path "express" "content" "docs" "s20_draw_mask" "mask0.svg"))
(define-runtime-path mask_bits_1_file (build-path "express" "content" "docs" "s20_draw_mask" "mask1.svg"))
(define-runtime-path mask_bits_2_file (build-path "express" "content" "docs" "s20_draw_mask" "mask2.svg"))
(define-runtime-path mask_bits_3_file (build-path "express" "content" "docs" "s20_draw_mask" "mask3.svg"))
(define-runtime-path mask_bits_4_file (build-path "express" "content" "docs" "s20_draw_mask" "mask4.svg"))
(define-runtime-path mask_bits_5_file (build-path "express" "content" "docs" "s20_draw_mask" "mask5.svg"))
(define-runtime-path mask_bits_6_file (build-path "express" "content" "docs" "s20_draw_mask" "mask6.svg"))
(define-runtime-path mask_bits_7_file (build-path "express" "content" "docs" "s20_draw_mask" "mask7.svg"))
(define-runtime-path mask_and_format_file (build-path "express" "content" "docs" "s21_draw_mask_and_format" "mask_and_format.svg"))
(define-runtime-path qr_file (build-path "express" "content" "docs" "s22_qr" "qr.svg"))

(define (qr-write-express data file_name
                          #:mode [mode 'B]
                          #:error_level [error_level 'H]
                          #:module_width [module_width 5]
                          #:color [color '("black" . "white")]
                          #:output_type [output_type 'png]
                          )
  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "# Explain each step of QR code write\n\n")
      
      (printf "## qr-write\n")

      (printf "## encoding data:\n")
      
      (printf "\n    ~a\n\n" data)

      (printf "## Options:\n")
      
      (printf "1. output file name: **~a**\n" file_name)
      (printf "2. mode: **~a**[default: 'B]\n" mode)
      (printf "3. error level: **~a**[default: ~a]\n" error_level 'H)
      (printf "4. module width: **~a**[default: ~a]\n" module_width 5)
      (printf "5. color: **~a**[default: ~a]\n" color '("black" . "white"))
      (printf "6. output image type: **~a**[default: ~a]\n" output_type "'png")
      ))

  (let ([qr (new-qr data module_width mode error_level (car color) (cdr color))])
    (get-version-express qr)

    (version-to-modules-express qr)

    (fill-points-color (QR-matrix qr) (MATRIX-points (QR-matrix qr)) '("grey" "white"))

    (fill-points-color (QR-matrix qr)
                       (get-points-between
                        (cons QUIET_ZONE_BRICKS QUIET_ZONE_BRICKS)
                        (cons (- (MATRIX-bricks (QR-matrix qr)) QUIET_ZONE_BRICKS 1) (- (MATRIX-bricks (QR-matrix qr)) QUIET_ZONE_BRICKS 1))
                        #:direction 'cross)
                       '(pattern1))
    (draw (QR-matrix qr) init_file 'svg)

    (draw-finder-pattern qr)
    (fill-type-points 'finder '("#32CD32" . "#ADFF2F") qr)
    (finder-pattern-express qr)
    (draw (QR-matrix qr) finder_pattern_file 'svg)

    (draw-separator qr)
    (fill-type-points 'separator '("black" . "orange") qr)
    (separator-express qr)
    (draw (QR-matrix qr) separator_file 'svg)

    (draw-timing-pattern qr)
    (fill-type-points 'timing '("#FF00FF" . "#DDA0DD") qr)
    (timing-pattern-express qr)
    (draw (QR-matrix qr) timing_pattern_file 'svg)

    (draw-alignment-pattern qr)
    (fill-type-points 'alignment '("#1E90FF" . "#B0C4DE") qr)
    (alignment-pattern-express qr)
    (draw (QR-matrix qr) alignment_pattern_file 'svg)

    (draw-dark-module qr)
    (fill-type-points 'dark '("#330099" . "#330099") qr)
    (dark-module-express qr)
    (draw (QR-matrix qr) dark_module_file 'svg)

    (draw-format-information "101010101010101" qr)
    (fill-type-points 'format '("#1E8449" . "#D4EFDF") qr)
    (format-information-express qr)
    (draw (QR-matrix qr) format_information_file 'svg)

    (draw-version-information qr)
    (fill-type-points 'version '("#0E29F0" . "#5866C8") qr)
    (version-information-express qr)
    (draw (QR-matrix qr) version_information_file 'svg)

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
      (s10-data-encoding-express s1_data_bits qr)

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

      (s11-head-bits-express s2_character_count s2_1_count_bit_width s3_character_count_indicator s4_mode_indicator s5_header_added_bits qr)

      ;; add terminator
      (set! s6_capacity_char_count (get-bits-width (QR-version qr) (QR-error_level qr)))

      (set! s7_capacity_bits_width (* 8 s6_capacity_char_count))

      (set! s8_terminator_appended_bits (add-terminator s5_header_added_bits s7_capacity_bits_width))

      (s12-terminator-express s6_capacity_char_count s7_capacity_bits_width s8_terminator_appended_bits qr)

      ;; add multiple eight
      (set! s9_multiple8_bits (add-multi-eight s8_terminator_appended_bits))

      (s13-padding-multiple8-express s9_multiple8_bits qr)

      ;; repeat padding
      (let ([repeat_str "1110110000010001"])
        (set! s10_repeat_pad_bits (repeat-right-pad-string s9_multiple8_bits s7_capacity_bits_width "1110110000010001"))

        (s14-repeat-pad-bits-express repeat_str s7_capacity_bits_width s10_repeat_pad_bits qr)
        )

      ;; split to groups

      ;; to decimal list
      (set! s11_decimal_list (split-bit-string-to-decimal s10_repeat_pad_bits))

      ;; group data
      (set! s12_split_contract (get-group-width (QR-version qr) (QR-error_level qr)))
          
      ;; split decimal list on contract
      (set! s13_origin_data_group (split-decimal-list-on-contract s11_decimal_list s12_split_contract))
      
      (s15-split-to-groups-express s11_decimal_list s12_split_contract s13_origin_data_group qr)

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

      (s16-error-correction-express s14_ec_count s16_error_code_group qr)

      ;; interleave data group
      (set! s17_interleave_data_group (interleave-data-group s16_error_code_group))
          
      (set! s18_interleave_data_bits (decimal-list-to-string s17_interleave_data_group))

      (s17-interleave-data-group-express s17_interleave_data_group s18_interleave_data_bits qr)

      ;; padded remainder bits
      (set! s19_remainder_bits_width (get-remainder-bits (QR-version qr)))

      (set! s20_padded_remainder_bits
            (~a s18_interleave_data_bits #:min-width (+ (string-length s18_interleave_data_bits) s19_remainder_bits_width) #:right-pad-string "0"))

      (s18-add-remainder-bits-express s19_remainder_bits_width s20_padded_remainder_bits qr)

      ;; fill data bits, skip reserved points
      (set! s22_trace_list (get-data-socket-list (QR-modules qr) #:skip_points_hash (QR-point_type_map qr)))

      (draw-data s20_padded_remainder_bits s22_trace_list qr)
      (fill-type-points 'data '("#2F4F4F" . "#C0C0C0") qr)
      (s19-draw-data-bits-express s20_padded_remainder_bits s22_trace_list qr)
      (draw (QR-matrix qr) data_bits_file 'svg)
      
      ;; mask func showcase
      (let ([mask_qr (new-qr data module_width mode error_level (car color) (cdr color))]
            [mask_list (map
                        (lambda (mask_number)
                          (let ([mask_points_map (hash-copy (QR-point_val_map mask_qr))])
                            (for-each
                             (lambda (item)
                               (hash-set! mask_points_map (car item) (cdr item)))
                             (mask-func data_list mask_number))
                            mask_points_map))
                        '(0 1 2 3 4 5 6 7))])

        (let loop ([mask_index 0])
          (when (< mask_index 8)
            (set-QR-point_val_map! mask_qr (list-ref mask_list mask_index))
            (fill-type-points 'data '("#2F4F4F" . "#C0C0C0") qr)
            (draw (QR-matrix qr)
                  (cond
                   [(= mask_index 0) mask_showcase_bits_0_file]
                   [(= mask_index 1) mask_showcase_bits_1_file]
                   [(= mask_index 2) mask_showcase_bits_2_file]
                   [(= mask_index 3) mask_showcase_bits_3_file]
                   [(= mask_index 4) mask_showcase_bits_4_file]
                   [(= mask_index 5) mask_showcase_bits_5_file]
                   [(= mask_index 6) mask_showcase_bits_6_file]
                   [(= mask_index 7) mask_showcase_bits_7_file])
                  'svg)
            (loop (add1 mask_index))))

        (fill-points-color (QR-matrix mask_qr) (MATRIX-points (QR-matrix mask_qr)) '("black" "white"))
        (draw (QR-matrix mask_qr) mask_init_file 'svg)

        (s20s-draw-mask-showcase-express mask_list qr)
        )

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
        
        (s20-draw-mask-express mask_list condition_list penalty_list min_penalty mask_index qr)
        (fill-type-points 'data '("#2F4F4F" . "#C0C0C0") qr)
        (draw (QR-matrix qr) mask_original_file 'svg)

        (let loop ([mask_index 0])
          (when (< mask_index 8)
            (set-QR-point_val_map! qr (list-ref mask_list mask_index))
            (fill-type-points 'data '("#2F4F4F" . "#C0C0C0") qr)
            (draw (QR-matrix qr)
                  (cond
                   [(= mask_index 0) mask_bits_0_file]
                   [(= mask_index 1) mask_bits_1_file]
                   [(= mask_index 2) mask_bits_2_file]
                   [(= mask_index 3) mask_bits_3_file]
                   [(= mask_index 4) mask_bits_4_file]
                   [(= mask_index 5) mask_bits_5_file]
                   [(= mask_index 6) mask_bits_6_file]
                   [(= mask_index 7) mask_bits_7_file])
                  'svg)
            (loop (add1 mask_index))))

        ;; draw selected mask
        (set-QR-point_val_map! qr (list-ref mask_list mask_index))
        (fill-type-points 'data '("#2F4F4F" . "#C0C0C0") qr)
        (set! format_str (hash-ref (get-error-code-hash) (format "~a-~a" error_level mask_index)))

        (draw-format-information format_str qr)
        (fill-type-points 'format '("#1E8449" . "#D4EFDF") qr)
        (s21-draw-mask-and-format-express error_level mask_index format_str qr)
        (draw (QR-matrix qr) mask_and_format_file 'svg)

        (s22-qr-express qr)
        (fill-points-color (QR-matrix qr) (MATRIX-points (QR-matrix qr)) '("white" "white"))
        (fill-type-points 'all '("black" . "white") qr)
        (draw (QR-matrix qr) qr_file 'svg)
        )
      )
    )
  )

(delete-directory/files (build-path "express" "content"))
(make-directory* (build-path "express" "content" "docs"))

;(qr-write-express "Life is too short to put up unnecessory stress on everyday, you must work in a place that fuel your personal passion." "chenxiao.svg" #:module_width 20 #:output_type 'svg)

(qr-write-express "Hello world!" "chenxiao.svg" #:module_width 20 #:output_type 'svg)
