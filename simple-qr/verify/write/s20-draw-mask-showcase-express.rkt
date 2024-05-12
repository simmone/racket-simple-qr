#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path mask_showcase_init_file (build-path "express" "content" "docs" "s20_mask_showcase" "original.svg"))
(define-runtime-path s20_draw-mask-showcase_directory (build-path 'up "express" "content" "docs" "s20_mask_showcase"))
(define-runtime-path mask_showcase_bits_0_file (build-path "express" "content" "docs" "s20_mask_showcase" "mask0.svg"))
(define-runtime-path mask_showcase_bits_1_file (build-path "express" "content" "docs" "s20_mask_showcase" "mask1.svg"))
(define-runtime-path mask_showcase_bits_2_file (build-path "express" "content" "docs" "s20_mask_showcase" "mask2.svg"))
(define-runtime-path mask_showcase_bits_3_file (build-path "express" "content" "docs" "s20_mask_showcase" "mask3.svg"))
(define-runtime-path mask_showcase_bits_4_file (build-path "express" "content" "docs" "s20_mask_showcase" "mask4.svg"))
(define-runtime-path mask_showcase_bits_5_file (build-path "express" "content" "docs" "s20_mask_showcase" "mask5.svg"))
(define-runtime-path mask_showcase_bits_6_file (build-path "express" "content" "docs" "s20_mask_showcase" "mask6.svg"))
(define-runtime-path mask_showcase_bits_7_file (build-path "express" "content" "docs" "s20_mask_showcase" "mask7.svg"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s20_mask_showcase" "_index.md"))

(provide (contract-out
          [s20-draw-mask-showcase-express (-> list? QR? void?)]))

(define (s20-draw-mask-showcase-express mask_list qr)
  (make-directory* s20_draw-mask-showcase_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 20\n")
      (printf "title: \"Step20: Mask Func\"\n")
      (printf "---\n\n")
      
      (printf "# Mask Func\n\n")

      (printf "## original image:\n\n")
      (printf "![original image](original.svg \"init image\")\n")

      (let loop ([mask_index 0])
        (when (< mask_index 8)
          (printf "## mask ~a:\n\n" mask_index)
          (printf "mask func is: ~a\n\n"
                  (cond
                   [(= mask_index 0) "(= (modulo (+ row column) 2) 0)"]
                   [(= mask_index 1) "(= (modulo row 2) 0)"]
                   [(= mask_index 2) "(= (modulo column 3) 0)"]
                   [(= mask_index 3) "(= (modulo (+ row column) 3) 0)"]
                   [(= mask_index 4) "(= (modulo (+ (floor (/ row 2)) (floor (/ column 3))) 2) 0)"]
                   [(= mask_index 5) "(= (+ (modulo (* row column) 2) (modulo (* row column) 3)) 0)"]
                   [(= mask_index 6) "(= (modulo (+ (modulo (* row column) 2) (modulo (* row column) 3)) 2) 0)"]
                   [(= mask_index 7) "(= (modulo (+ (modulo (+ row column) 2) (modulo (* row column) 3)) 2) 0)"]))
          (printf "![mask 0 image](mask~a.svg \"Mask 0\")\n" mask_index)
          (loop (add1 mask_index))
          ))
      )))
