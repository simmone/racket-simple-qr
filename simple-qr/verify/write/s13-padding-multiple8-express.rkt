#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s13_padding_multiple8_directory (build-path 'up "express" "content" "docs" "s13_padding_multiple8"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s13_padding_multiple8" "_index.md"))

(provide (contract-out
          [s13-padding-multiple8-express (-> string? QR? void?)]))

(define (s13-padding-multiple8-express multiple8_bits qr)
  (make-directory* s13_padding_multiple8_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 13\n")
      (printf "title: \"Step13: Padding multiple 8 0s\"\n")
      (printf "---\n\n")

      (printf "# Padding multiple 8 0s\n\n")

      (printf "After adding the terminator, if the number of bits in the string is not a multiple of 8, first pad the string
on the right with 0s to make the string's length a multiple of 8.n\n")

      (printf "## append 0s to the end let data bits is multiple 8:\n\n")
      (printf "~a\n" (bits-to-markdown-table multiple8_bits 8)))))
