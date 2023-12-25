#lang racket

(require racket/runtime-path)
(define-runtime-path s2_module_directory "../express/content/docs/s2-module")
(define-runtime-path index_md_file "../express/content/docs/s2-module/_index.md")
(define-runtime-path init_bitmap_file "../express/content/docs/s2-module/init.svg")

(require "../../share/draw/draw.rkt")

(provide (contract-out
          [version-to-modules-express (-> natural? natural? void?)]))

(define (version-to-modules-express version modules)
  (make-directory* s2_module_directory)

  (draw modules 10 (make-hash) (make-hash) '("gray" . "gray") init_bitmap_file 'svg)

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
      
      (printf "## init image:\n\n")
      (printf "![init image](init.svg \"Init Svg Image\")\n")
      )))

