#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path data_bits_file (build-path 'up "express" "content" "docs" "s19_draw_data_bits" "data_bits.svg"))
(define-runtime-path s20_draw-mask_directory (build-path 'up "express" "content" "docs" "s20_draw_mask"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s20_draw_mask" "_index.md"))

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
      (printf "![data bits image](original.svg \"Original Data Bits Filled\")\n")

      (let loop ([mask_index 0])
        (when (< mask_index 8)
          (printf "## mask ~a:\n\n" mask_index)
          (printf "mask func is: ~a\n\n"
                  (cond
                   [(= mask_index 0) "(= (modulo (+ row column) 2) 0)"]
                   [(= mask_index 1) "(= (modulo row 2) 0)"]
                   [(= mask_index 2) "
                   [(= mask_index 3) "
                   [(= mask_index 4) "
                   [(= mask_index 5) "
                   [(= mask_index 6) "
                   [(= mask_index 7) "

      (printf "penalty score = condition1[~a] + condition2[~a] + condition3[~a] + condition4[~a] = ~a\n\n"
              (list-ref (list-ref condition_list 0) 0)
              (list-ref (list-ref condition_list 0) 1)
              (list-ref (list-ref condition_list 0) 2)
              (list-ref (list-ref condition_list 0) 3)
              (list-ref penalty_list 0))
      (printf "![mask 0 image](mask0.svg \"Mask 0\")\n")
      )))
