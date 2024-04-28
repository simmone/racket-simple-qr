#lang racket

(require racket/runtime-path
         "../../share/qr.rkt")

(define-runtime-path s3_finder_pattern_directory (build-path 'up "express" "content" "docs" "s3_finder_pattern"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s3_finder_pattern" "_index.md"))

(provide (contract-out
          [finder-pattern-express (-> QR? void?)]))

(define (finder-pattern-express qr)
  (make-directory* s3_finder_pattern_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 3\n")
      (printf "title: \"Step3: Finder pattern\"\n")
      (printf "---\n\n")
      
      (printf "# Draw Finder Pattern\n\n")
      
      (printf "## Locate 3 finder patterns:\n\n")
      (printf "![finder pattern image](finder_pattern.svg \"Finder Pattern Filled\")\n")
      )))

