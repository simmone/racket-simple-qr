#lang racket

(require "../../share/qr.rkt"
         racket/runtime-path)

(define-runtime-path s2_module_directory (build-path 'up "express" "content" "docs" "s2_module"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s2_module" "_index.md"))

(provide (contract-out
          [version-to-modules-express (-> QR? void?)]))

(define (version-to-modules-express qr)
  (make-directory* s2_module_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 2\n")
      (printf "title: \"Step2: Modules(bricks) count\"\n")
      (printf "---\n\n")
      
      (printf "# Decide modules from version\n\n")
      
      (printf "## modules\n\n")
      (printf "modules = (+ 21 (* 4 (sub1 ~a))) = ~a\n\n" (QR-version qr) (QR-modules qr))
      (printf "flowing steps will fill a **~a**X**~a** points map\n\n" (QR-modules qr) (QR-modules qr))
      
      (printf "## init image:\n\n")
      (printf "![init image](init.svg \"Init Svg Image\")\n")
      )))

