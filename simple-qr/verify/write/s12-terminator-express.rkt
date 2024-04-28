#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s12_terminator_directory (build-path 'up "express" "content" "docs" "s12_terminator"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s12_terminator" "_index.md"))

(provide (contract-out
          [s12-terminator-express (-> natural? natural? string? QR? void?)]))

(define (s12-terminator-express capacity_char_count capacity_bits_width terminator_appendeded_bits qr)
  (make-directory* s12_terminator_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 13\n")
      (printf "title: \"Step13: Append terminator\"\n")
      (printf "---\n\n")
      
      (printf "# Append terminator at the end\n\n")

      (printf "If the bit string is shorter than the total number of required bits, a terminator of up to four 0s must be
added to the right side of the string. If the bit string is more than four bits shorter than the required
number of bits, add four 0s to the end. If the bit string is fewer than four bits shorter, add only the
number of 0s that are needed to reach the required number of bits.\n\n")

      (printf "## charactor capacity is [~a] decided by version and error level.\n\n" capacity_char_count)
      
      (printf "## capacity bits = capacity count X 8 = [~a]\n\n" capacity_bits_width)

      (printf "## append terminator to the end:\n\n")
      (printf "~a\n" (bits-to-markdown-table terminator_appendeded_bits 8)))))
