#lang racket

(require "version-express.rkt")

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
                                 any)]
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

      (printf "## encoding data:\n")
      
      (printf "\n    ~a\n\n" data)

      (printf "## Options:\n")
      
      (printf "1. output file name: **~a**\n" file_name)
      (printf "2. mode: **~a**\n" mode)
      (printf "3. error level: **~a**\n" error_level)
      (printf "4. module width: **~a**\n" module_width)
      (printf "5. color: **~a**\n" color)
      (printf "6. output image type: **~a**\n" output_type)
      ))

  (define version (get-version (string-length data) mode error_level))
  (get-version-express (string-length data) mode error_level)
  )
