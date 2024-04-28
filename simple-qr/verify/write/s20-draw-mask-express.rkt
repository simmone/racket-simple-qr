#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path data_bits_file (build-path 'up "express" "content" "docs" "s19_draw_data_bits" "data_bits.svg"))
(define-runtime-path s20_draw-mask_directory "../express/content/docs/s20_draw_mask")
(define-runtime-path index_md_file "../express/content/docs/s20_draw_mask/_index.md")

(provide (contract-out
          [s20-draw-mask-express (-> list? list? list? QR? void?)]))

(define (s20-draw-mask-express mask_list condition_list penalty_list  qr)
  (make-directory* s20_draw-mask_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 20\n")
      (printf "title: \"Step20: Mask data\"\n")
      (printf "---\n\n")
      
      (printf "# Mask data\n\n")

      (printf "The purpose of this step is to modify the QR code to make it as easy for a QR code reader to scan as possible.\n\n")

      (printf "The QR code specification defines eight mask patterns (mask-patterns) that can be applied to the QR
code. For example, for mask pattern #1, every even-numbered row in the QR matrix is masked, and
for mask pattern #2, every third column in the QR matrix is masked.\n\n")

      (printf "After a mask pattern has been applied to the QR matrix, it is given a penalty score based on four
evaluation conditions that are defined in the QR code specification. A QR code encoder must apply all
eight mask patterns and evaluate each one. Whichever mask pattern results in the lowest penalty
score is the mask pattern that must be used for the final output.\n\n")

      (printf "## original image:\n\n")
      (printf "![data bits image](../mask.svg \"Data Bits Filled\")\n")
      )))
