#lang racket

(require racket/runtime-path
         "../../share/qr.rkt")

(define-runtime-path s7_dark_module_directory "../express/content/docs/s7_dark_module")
(define-runtime-path index_md_file "../express/content/docs/s7_dark_module/_index.md")

(provide (contract-out
          [dark-module-express (-> QR? void?)]))

(define (dark-module-express qr)
  (make-directory* s7_dark_module_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 7\n")
      (printf "title: \"Step7: Dark_module\"\n")
      (printf "---\n\n")
      
      (printf "# Draw Dark_Module\n\n")
      
      (printf "## dark_module\n\n")
      (printf "The dark module is a single black module that is always placed beside the bottom left finder pattern.
The sections below explain in greater detail how to position the function patterns.More specifically, the dark module is always located at the coordinate ([(4 * V) + 9], 8) where V
is the version of the QR code.\n\n")
      (printf "![dark_module image](dark_module.svg \"Dark_Module\")\n")
      )))

