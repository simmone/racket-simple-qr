#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s15_split_to_groups_directory (build-path 'up "express" "content" "docs" "s15_split_to_groups"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s15_split_to_groups" "_index.md"))

(provide (contract-out
          [s15-split-to-groups-express (-> list? list? list? QR? void?)]))

(define (s15-split-to-groups-express  decimal_list split_contract origin_data_groups qr)
  (make-directory* s15_split_to_groups_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 15\n")
      (printf "title: \"Step15: Split data to groups\"\n")
      (printf "---\n\n")

      (printf "# Split data to groups\n\n")

      (printf "Before generating error correction codewords, it may be necessary to break up the data codewords
into smaller blocks if the QR code is larger than version 2. \n\n")

      (printf "## convert bit string to byte list: \n\n")
      (printf "~a\n" (list-to-markdown-table decimal_list 8))

      (printf "## [~a-~a]'s split contract is [~a]\n\n" (QR-version qr) (QR-error_level qr) split_contract)
      (printf "split contract's meaning: \n\n(group1_block_count group1_byte_count_per_block)\n\n(group2_block_count group2_byte_count_per_block)\n\n")

      (printf "## split to groups by contract: \n\n")

      (printf "group1: \n\n")
      (printf "~a\n" (list-to-markdown-table (first origin_data_groups) 1))

      (printf "group2: \n\n")
      (printf "~a\n" (list-to-markdown-table (second origin_data_groups) 1))
      )))
