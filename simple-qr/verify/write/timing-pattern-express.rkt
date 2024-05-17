#lang racket

(require racket/runtime-path
         "../../share/qr.rkt")

(define-runtime-path s5_timing_pattern_directory (build-path 'up "express" "content" "docs" "s5_timing_pattern"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s5_timing_pattern" "_index.md"))

(provide (contract-out
          [timing-pattern-express (-> QR? void?)]))

(define (timing-pattern-express qr)
  (make-directory* s5_timing_pattern_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 5\n")
      (printf "title: \"Step5: Timing-pattern\"\n")
      (printf "---\n\n")

      (printf "# Draw Timing-Pattern\n\n")

      (printf "## timing-pattern\n\n")
      (printf "The horizontal and vertical Timing Patterns respectively consist of a one module wide row or column of alternating
dark and light modules, commencing and ending with a dark module. The horizontal Timing Pattern runs across
row 6 of the symbol between the separators for the upper Position Detection Patterns; the vertical Timing Pattern
similarly runs down column 6 of the symbol between the separators for the left-hand Position Detection Patterns.
They enable the symbol density and version to be determined and provide datum positions for determining module
coordinates.\n\n")
      (printf "![timing-pattern image](timing_pattern.svg \"Timing-Pattern\")\n")
      )))

