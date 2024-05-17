#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s18_add-remainder-bits_directory (build-path 'up "express" "content" "docs" "s18_add_remainder_bits"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s18_add_remainder_bits" "_index.md"))

(provide (contract-out
          [s18-add-remainder-bits-express (-> natural? string? QR? void?)]))

(define (s18-add-remainder-bits-express remainder_bits_width padded_remainder_bits qr)
  (make-directory* s18_add-remainder-bits_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 18\n")
      (printf "title: \"Step18: Add remainder bits\"\n")
      (printf "---\n\n")

      (printf "# Add remainder bits\n\n")

      (printf "For some QR versions, the final binary message is not long enough to fill the required number of bits.
In this case, it is necessary to add a certain number of 0s to the end of the final message to make it
have the correct length. These extra 0s are called remainder bits. A version 5 QR code, like the one
in this example, must have 7 remainder bits added to the end.\n\n")

      (printf "## need add [~a]'s remainder 0s bit.\n\n" remainder_bits_width)

      (printf "## after added remainder bits: \n\n")
      (printf "~a\n" (bits-to-markdown-table padded_remainder_bits 64))
      )))
