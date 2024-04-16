#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s14_repeat_pad_bits_directory "../express/content/docs/s14_repeat_pad_bits")
(define-runtime-path index_md_file "../express/content/docs/s14_repeat_pad_bits/_index.md")

(provide (contract-out
          [s14-repeat-pad-bits-express (-> string? natural? string? QR? void?)]))

(define (s14-repeat-pad-bits-express repeat_str capacity_bits_width repeat_pad_bits qr)
  (make-directory* s14_repeat_pad_bits_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 14\n")
      (printf "title: \"Step14: Add pad bytes\"\n")
      (printf "---\n\n")
      
      (printf "# Add pad bytes\n\n")

      (printf "If the string is still not long enough to fill the maximum capacity, add the following bytes to the end of
the string, repeating until the string has reached the maximum length:
11101100 00010001.\n\n")

      (printf "## capacity bits width: [~a]\n\n" capacity_bits_width)

      (printf "## repeat bytes: 11101100 00010001\n\n")

      (printf "## after repeat pad:\n\n")
      (printf "~a\n" (bits-to-markdown-table repeat_pad_bits 16)))))
