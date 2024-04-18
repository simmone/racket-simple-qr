#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s17_interleave-data-group_directory "../express/content/docs/s17_interleave_data_group")
(define-runtime-path index_md_file "../express/content/docs/s17_interleave_data_group/_index.md")

(provide (contract-out
          [s17-interleave-data-group-express (-> list? string? QR? void?)]))

(define (s17-interleave-data-group-express interleave_data_group interleave_data_bits qr)
  (make-directory* s17_interleave-data-group_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 17\n")
      (printf "title: \"Step17: Interleave data group\"\n")
      (printf "---\n\n")
      
      (printf "# Interleave data group\n\n")

      (printf "The blocks are interleaved by doing the following:\n")
      (printf "1. take the first data codeword from the first block\n")
      (printf "2. followed by the first data codeword from the second block\n")
      (printf "3. followed by the first data codeword from the third block\n")
      (printf "4. followed by the first data codeword from the fourth block\n")
      (printf "5. followed by the second data codeword from the first block\n")
      (printf "6. and so on\n")
      (printf "This pattern is repeated, going across the blocks, until all of the data codewords have been\n")
      (printf "interleaved.\n")
      (printf "After that, do the following:\n")
      (printf "1. take the first error correction codeword from the first block\n")
      (printf "2. followed by the first error correction codeword from the second block\n")
      (printf "3. followed by the first error correction codeword from the third block\n")
      (printf "4. followed by the first error correction codeword from the fourth block\n")
      (printf "5. followed by the second error correction codeword from the first block\n")
      (printf "6. and so on\n")
      (printf "Do this until all error correction codewords have been used up\n")

      (printf "## interleaved data list: \n\n")

      (printf "~a\n" (list-to-markdown-table interleave_data_group 8))

      (printf "## to bits: \n\n")
      (printf "~a\n" (bits-to-markdown-table interleave_data_bits 64))
      )))
