#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s22_draw-mask-and-format_directory (build-path 'up "express" "content" "docs" "s22_draw_mask_and_format"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s22_draw_mask_and_format" "_index.md"))

(provide (contract-out
          [s22-draw-mask-and-format-express (-> QR-ERROR-LEVEL? natural? string? QR? void?)]))

(define (s22-draw-mask-and-format-express error_level mask_index format_str qr)
  (make-directory* s22_draw-mask-and-format_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 22\n")
      (printf "title: \"Step22: Draw mask & format\"\n")
      (printf "---\n\n")

      (printf "# format string\n\n")

      (printf "from error_level[~a] and mask_index[~a], get format string: [~a]\n" error_level mask_index format_str)

      (printf "## write mask data and format string\n\n")
      (printf "![last image](mask_and_format.svg \"Mask and Format Bits Filled\")\n")
      )))
