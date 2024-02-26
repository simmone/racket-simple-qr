#lang racket

(require racket/runtime-path)
(define-runtime-path s3_finder_pattern_directory "../express/content/docs/s3_finder_pattern")
(define-runtime-path index_md_file "../express/content/docs/s3_finder_pattern/_index.md")
(define-runtime-path finder_pattern_bitmap_file "../express/content/docs/s3_finder_pattern/finder_pattern.svg")

(require "../../share/draw/draw.rkt")

(provide (contract-out
          [finder-pattern-express (-> natural? hash? void?)]))

(define (finder-pattern-express modules points_map)
  (make-directory* s3_finder_pattern_directory)

  (draw (CANVAS modules 10 points_map "black" "orange") finder_pattern_bitmap_file 'svg)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 3\n")
      (printf "title: \"Step3: finder pattern\"\n")
      (printf "---\n\n")
      
      (printf "# Draw Finder Pattern\n\n")
      
      (printf "## Locate 3 finder patterns:\n\n")
      (printf "![finder pattern image](finder_pattern.svg \"Finder Pattern Filled\")\n")
      )))

