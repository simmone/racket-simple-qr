#lang racket

(require racket/runtime-path)
(define-runtime-path index_md_file "../express/content/docs/s2-module/_index.md")

(provide (contract-out
          [version-to-modules-express (-> natural? natural? void?)]))

(define (version-to-modules-express version modules)
  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 2\n")
      (printf "title: \"Step2: modules(bricks) count\"\n")
      (printf "---\n\n")
      
      (printf "# Decide modules from version\n\n")
      
      (printf "## modules\n\n")
      (printf "modules = (+ 21 (* 4 (sub1 ~a))) = ~a\n\n" version modules)
      (printf "flowing steps will fill a **~a**X**~a** points map\n\n" modules modules)
      ))
  )
