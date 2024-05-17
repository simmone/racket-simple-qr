#lang racket

(require racket/runtime-path
         "../../share/qr.rkt")

(define-runtime-path s4_separator_directory (build-path 'up "express" "content" "docs" "s4_separator"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s4_separator" "_index.md"))

(provide (contract-out
          [separator-express (-> QR? void?)]))

(define (separator-express qr)
  (make-directory* s4_separator_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 4\n")
      (printf "title: \"Step4: Separator\"\n")
      (printf "---\n\n")

      (printf "# Draw Separator\n\n")

      (printf "## separator\n\n")
      (printf "separator is a light color band to separate finder pattern from other areas:\n\n")
      (printf "![separator image](separator.svg \"Separator\")\n")
      )))

