#lang racket

(require racket/runtime-path
         "../../share/qr.rkt")

(define-runtime-path s9_version_information_directory "../express/content/docs/s9_version_information")
(define-runtime-path index_md_file "../express/content/docs/s9_version_information/_index.md")

(provide (contract-out
          [version-information-express (-> QR? void?)]))

(define (version-information-express qr)
  (make-directory* s9_version_information_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 9\n")
      (printf "title: \"Step9: Version information\"\n")
      (printf "---\n\n")
      
      (printf "# Draw Version_Information\n\n")
      
      (printf "## version_information\n\n")
      (printf "If the QR Code is version 7 or larger, you must include an 18-bit version information string in the
bottom left and top right corners of the QR code. (For a full list of all possible version information
strings, refer to the format and version tables (format-version-tables).) The version information areas
are the 6x3 blue rectangles shown in the images below. The version information is placed beside the
finder patterns no matter how large the QR code is. \n\n")
      (printf "![version_information image](version_information.svg \"Version_Information\")\n")
      )))

