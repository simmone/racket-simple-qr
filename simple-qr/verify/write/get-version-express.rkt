#lang racket

(require racket/runtime-path)
(define-runtime-path index_md_file "../express/content/docs/s1_version/_index.md")

(provide (contract-out
          [get-version-express (->
                                natural?
                                string?
                                string?
                                natural?
                                void?)]))

(define (get-version-express char_count mode error_level version)
  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 1\n")
      (printf "title: \"Step1: Decide Version\"\n")
      (printf "---\n\n")
      
      (printf "# Decide version(capacity)\n")
      (printf "capacity controlled by char count, mode and error level\n\n")
      
      (printf "## Elements\n")
      (printf "1. char count: ~a\n" char_count)
      (printf "2. mode: ~a\n" mode)
      (printf "3. error_level: ~a\n" error_level)

      (printf "## Version\n")
      (printf "version is **~a**\n" version)
      ))
  )
