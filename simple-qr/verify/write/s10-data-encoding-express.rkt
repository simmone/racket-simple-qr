#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s10_data_encoding_directory (build-path 'up "express" "content" "docs" "s10_data_encoding"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s10_data_encoding" "_index.md"))

(provide (contract-out
          [s10-data-encoding-express (-> string? QR? void?)]))

(define (s10-data-encoding-express bits_str qr)
  (make-directory* s10_data_encoding_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 10\n")
      (printf "title: \"Step10: Data to bits\"\n")
      (printf "---\n\n")

      (printf "# Data to bits\n\n")

      (printf "## data_encoding\n\n")
      (printf "convert [~a] to bits on mode[~a]\n\n" (QR-data qr) (QR-mode qr))
      (printf "data string to bits:\n~a\n" (string-to-bits-markdown-table (QR-data qr) bits_str))
      )))

