#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s21_draw-mask-and-format_directory (build-path 'up "express" "content" "docs" "s21_draw_mask_and_format"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s21_draw_mask_and_format" "_index.md"))

(provide (contract-out
          [s21-draw-mask-and-format-express (-> string? natural? string? QR? void?)]))

(define (s21-draw-mask-and-format-express error_level mask_index format_str qr)
  (make-directory* s21_draw-mask-and-format_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 21\n")
      (printf "title: \"Step21: Draw mask data and format string\"\n")
      (printf "---\n\n")
      
      (printf "# format string\n\n")

      (printf "from error_level[~a] and mask_index[~a], get format string: [~a]\n" error_level mask_index format_str)

      (printf "## write mask data and format string\n\n")
      (printf "![last image](mask_and_format.svg \"Mask and Format Bits Filled\")\n")
      )))
