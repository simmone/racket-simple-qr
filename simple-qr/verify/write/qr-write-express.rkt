#lang racket

(require "../../share/qr.rkt"
         "../../share/draw/matrix.rkt"
         "../../share/draw/draw.rkt"
         "../../share/lib.rkt"
         "../../share/version.rkt"
         "get-version-express.rkt"
         "version-to-module-express.rkt"
         "../../write/finder-pattern.rkt"
         "finder-pattern-express.rkt"
         "../../write/separator.rkt"
         "separator-express.rkt"
         "../../write/timing-pattern.rkt"
         "timing-pattern-express.rkt"
         racket/runtime-path)

(define-runtime-path index_md_file "../express/content/_index.md")
(define-runtime-path init_file "../express/content/docs/s2_module/init.svg")
(define-runtime-path finder_pattern_file "../express/content/docs/s3_finder_pattern/finder_pattern.svg")
(define-runtime-path separator_file "../express/content/docs/s4_separator/separator.svg")
(define-runtime-path timing_pattern_file "../express/content/docs/s5_timing_pattern/timing_pattern.svg")

(provide (contract-out
          [qr-write-express (->* (string? path-string?) 
                                 (
                                  #:mode string?
                                         #:error_level string?
                                         #:module_width exact-nonnegative-integer?
                                         #:color (cons/c string? string?)
                                         #:output_type (or/c 'png 'svg)
                                         )
                                 void?)]
          ))

(define (qr-write-express data file_name
                          #:mode [mode "B"]
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
      (printf "2. mode: **~a**[default: ~a]\n" mode "B")
      (printf "3. error level: **~a**[default: ~a]\n" error_level "H")
      (printf "4. module width: **~a**[default: ~a]\n" module_width 5)
      (printf "5. color: **~a**[default: ~a]\n" color '("black" . "white"))
      (printf "6. output image type: **~a**[default: ~a]\n" output_type "'png")
      ))

  (let* ([qr (new-qr data module_width mode error_level (car color) (cdr color))])
    (get-version-express qr)

    (version-to-modules-express qr)

    (fill-points (QR-matrix qr) (MATRIX-points (QR-matrix qr)) '("grey" "white"))
    (fill-points (QR-matrix qr)
                 (get-points-between
                  (cons QUIET_ZONE_BRICKS QUIET_ZONE_BRICKS)
                  (cons (- (MATRIX-bricks (QR-matrix qr)) QUIET_ZONE_BRICKS 1) (- (MATRIX-bricks (QR-matrix qr)) QUIET_ZONE_BRICKS 1))
                  #:direction 'cross)
                 '(pattern1))
    (draw (QR-matrix qr) init_file 'svg)

    (draw-finder-pattern qr)
    (fill-type-points "finder" '("black" . "white") qr)
    (finder-pattern-express qr)
    (draw (QR-matrix qr) finder_pattern_file 'svg)

    (draw-separator qr)
    (fill-type-points "separator" '("black" . "orange") qr)
    (separator-express qr)
    (draw (QR-matrix qr) separator_file 'svg)

    (draw-timing-pattern qr)
    (fill-type-points "timing" '("#FF00FF" . "#DDA0DD") qr)
    (timing-pattern-express qr)
    (draw (QR-matrix qr) timing_pattern_file 'svg)
    )
  )
