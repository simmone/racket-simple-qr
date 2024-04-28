#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s16_error_correction_directory (build-path 'up "express" "content" "docs" "s16_error_correction"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s16_error_correction" "_index.md"))

(provide (contract-out
          [s16-error-correction-express (-> natural? list? QR? void?)]))

(define (s16-error-correction-express ec_count error_code_group qr)
  (make-directory* s16_error_correction_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 16\n")
      (printf "title: \"Step16: Error correction encoding\"\n")
      (printf "---\n\n")
      
      (printf "# Error correction encoding\n\n")

      (printf "Error correction codewords allow QR code readers to detect and correct errors in QR codes.The error correction codewords will be generated using a method called Reed-Solomon Error correction..\n\n")

      (printf "## calculate error correction count \n\n")
      (printf "[~a-~a]'s error count = [~a]\n" (QR-version qr) (QR-error_level qr) ec_count)

      (printf "## generate each group's error correction bytes: \n\n")

      (printf "group1 data and error code: \n\n")
      (printf "~a\n" (list-to-markdown-table (first error_code_group) 1))

      (printf "group2 data and error code: \n\n")
      (printf "~a\n" (list-to-markdown-table (second error_code_group) 1))
      )))
