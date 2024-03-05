#lang racket

(require "../../share/qr.rkt")
(require "../../share/version.rkt")

(require "get-version-express.rkt")
(require "version-to-module-express.rkt")

(require "../../write/finder-pattern.rkt")
(require "finder-pattern-express.rkt")

(require racket/runtime-path)
(define-runtime-path index_md_file "../express/content/_index.md")

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

    (draw-finder-pattern qr)
    (finder-pattern-express qr))
  )
