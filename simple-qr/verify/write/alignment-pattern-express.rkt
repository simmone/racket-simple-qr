#lang racket

(require racket/runtime-path
         "../../share/qr.rkt")

(define-runtime-path s6_alignment_pattern_directory (build-path 'up "express" "content" "docs" "s6_alignment_pattern"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s6_alignment_pattern" "_index.md"))

(provide (contract-out
          [alignment-pattern-express (-> QR? void?)]))

(define (alignment-pattern-express qr)
  (make-directory* s6_alignment_pattern_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 6\n")
      (printf "title: \"Step6: Alignment-pattern\"\n")
      (printf "---\n\n")
      
      (printf "# Draw Alignment-Pattern\n\n")
      
      (printf "## alignment-pattern\n\n")
      (printf "Each Alignment Pattern may be viewed as three superimposed concentric squares and is constructed of dark 5  5
modules, light 3  3 modules and a single central dark module. The number of Alignment Patterns depends on the
symbol version and they shall be placed in all Model 2 symbols of Version 2 or larger.\n\n")
      (printf "![alignment-pattern image](alignment_pattern.svg \"Alignment-Pattern\")\n")
      )))

