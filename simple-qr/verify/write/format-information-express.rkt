#lang racket

(require racket/runtime-path
         "../../share/qr.rkt")

(define-runtime-path s8_format_information_directory (build-path 'up "express" "content" "docs" "s8_format_information"))
(define-runtime-path index_md_file (build-path 'up "express" "content" "docs" "s8_format_information" "_index.md"))

(provide (contract-out
          [format-information-express (-> QR? void?)]))

(define (format-information-express qr)
  (make-directory* s8_format_information_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 8\n")
      (printf "title: \"Step8: Format information\"\n")
      (printf "---\n\n")
      
      (printf "# Draw Format_Information\n\n")
      
      (printf "## format_information\n\n")
      (printf "A strip of modules beside the separators must be reserved for the format information area as follows:\n\n")
      (printf " - Near the top-left finder pattern, a one-module strip must be reserved below and to the right of
the separator.\n\n")
      (printf " - Near the top-right finder pattern, a one-module strip must be reserved below the separator.\n\n")
      (printf " - Near the bottom-left finder pattern, a one-module strip must be reserved to the right of the
separator.\n\n")

      (printf "## reserved format information\n\n")
      (printf "use 111100011011100 to reserve the area.\n\n")
      (printf "![format_information image](format_information.svg \"Format_Information\")\n")
      )))

