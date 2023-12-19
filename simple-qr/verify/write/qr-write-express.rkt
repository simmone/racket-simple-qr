#lang racket

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
    (lambda ()
      (printf "# Express each step of QR code write\n\n"))))
