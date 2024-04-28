#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s19_draw-data-bits_directory (build-path 'up "express" "content" "docs" "s19_draw_data_bits"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s19_draw_data_bits" "_index.md"))

(provide (contract-out
          [s19-draw-data-bits-express (-> string? list? QR? void?)]))

(define (s19-draw-data-bits-express padded_remainder_bits s22_trace_list qr)
  (make-directory* s19_draw-data-bits_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 19\n")
      (printf "title: \"Step19: Draw data bits\"\n")
      (printf "---\n\n")
      
      (printf "# Draw data bits\n\n")

      (printf "The data bits are placed starting at the bottom-right of the matrix and proceeding upward in a column
that is 2 modules wide. Use white pixels for 0, and black pixels for 1. When the column reaches the
top, the next 2-module column starts immediately to the left of the previous column and continues
downward. Whenever the current column reaches the edge of the matrix, move on to the next 2-
module column and change direction. If a function pattern or reserved area is encountered, the data
bit is placed in the next unused module.\n\n")

      (printf "## bit and pos table: \n\n")
      (printf "~a\n" (dual-list-to-markdown-table (string->list padded_remainder_bits) s22_trace_list 8))

      (printf "## draw data bits:\n\n")
      (printf "![data bits image](data_bits.svg \"Data Bits Filled\")\n")
      )))
