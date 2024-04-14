#lang racket

(require racket/runtime-path
         "../share/qr.rkt"
         "../share/draw/matrix.rkt"
         "../share/draw/draw.rkt"
         "../share/lib.rkt"
         "../share/version.rkt"
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
         "../write/data-encoding/data-encoding.rkt"
         "write/s10-data-encoding-express.rkt"
         "write/s11-head-bits-express.rkt"
         "../share/bits-width.rkt"
         "write/s12-terminator-express.rkt"
         racket/runtime-path)

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

(define (qr-write-express data file_name
                          #:mode [mode 'B]
                          #:error_level [error_level "H"]
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
      (printf "3. error level: **~a**[default: ~a]\n" error_level "H")
      (printf "4. module width: **~a**[default: ~a]\n" module_width 5)
      (printf "5. color: **~a**[default: ~a]\n" color '("black" . "white"))
      (printf "6. output image type: **~a**[default: ~a]\n" output_type "'png")
      ))

  (let* ([qr (new-qr data module_width mode error_level (car color) (cdr color))])
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

    (draw-format-information "111100011011100" qr)
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
          [s21_data_list #f]
          [s22_trace_list #f]
          )

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
      )
    )
  )


(make-directory* (build-path "express" "content" "docs"))

;(qr-write-express "Life is too short to put up unnecessory stress on everyday, you must work in a place that fuel your personal passion." "chenxiao.svg" #:module_width 20 #:output_type 'svg)

(qr-write-express "chenxiao" "chenxiao.svg" #:module_width 20 #:output_type 'svg)
