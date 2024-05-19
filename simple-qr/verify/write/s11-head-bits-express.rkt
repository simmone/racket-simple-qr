#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s11_head_bits_directory (build-path 'up "express" "content" "docs" "s11_head_bits"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s11_head_bits" "_index.md"))

(provide (contract-out
          [s11-head-bits-express (-> natural? natural? string? string? string? QR? void?)]))

(define (s11-head-bits-express character_count count_bit_width character_count_indicator s4_mode_indicator s5_header_added_bits qr)
  (make-directory* s11_head_bits_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 11\n")
      (printf "title: \"Step11: Head bits\"\n")
      (printf "---\n\n")

      (printf "# Add head bits\n\n")

      (printf "## mode indicator\n\n")
      (printf "mode indicator:[~a]\n\n" s4_mode_indicator)

      (printf "## character count bits\n\n")
      (printf "character count:[~a]\n\n" character_count)
      (printf "character count bit width:[~a]\n\n" count_bit_width)
      (printf "character count bits:[~a]\n\n" character_count_indicator)

      (printf "## concat mode, character count indicator, data bits\n\n")
      (printf "data bits:\n~a\n" (bits-to-markdown-table s5_header_added_bits 8)))))

