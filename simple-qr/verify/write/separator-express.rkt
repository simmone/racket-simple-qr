#lang racket

(require racket/runtime-path)
(define-runtime-path s4_separator_directory "../express/content/docs/s4_separator")
(define-runtime-path index_md_file "../express/content/docs/s4_separator/_index.md")
(define-runtime-path separator_bitmap_file "../express/content/docs/s4_separator/separator.svg")

(require "../../share/draw/draw.rkt")

(provide (contract-out
          [separator-express (-> natural? hash? void?)]))

(define (separator-express modules points_map)
  (make-directory* s4_separator_directory)

  (draw (CANVAS modules 10 points_map "black" "orange") separator_bitmap_file 'svg)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 4\n")
      (printf "title: \"Step4: separator\"\n")
      (printf "---\n\n")
      
      (printf "# Draw Separator\n\n")
      
      (printf "## separator\n\n")
      (printf "separator is a light color band to separate finder pattern from other areas:\n\n")
      (printf "![separator image](separator.svg \"Separator\")\n")
      )))

