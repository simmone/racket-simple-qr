#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s22_qr_directory (build-path 'up "express" "content" "docs" "s22_qr"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s22_qr" "_index.md"))

(provide (contract-out
          [s22-qr-express (-> QR? void?)]))

(define (s22-qr-express qr)
  (make-directory* s22_qr_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 22\n")
      (printf "title: \"Step22: QR code\"\n")
      (printf "---\n\n")
      
      (printf "# QR code\n\n")

      (printf "![qr_image](qr.svg \"Result\")\n")
      )))
