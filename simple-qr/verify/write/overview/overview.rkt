#lang racket

(provide (contract-out
          [write-report-overview (-> path-string? void?)]
          ))

(require racket/runtime-path)
(define-runtime-path overview_png "overview.png")

(define (write-report-overview express_path)
  (let* ([scrbl_dir (build-path express_path "overview")]
         [scrbl_file (build-path scrbl_dir "overview.scrbl")])

    (with-output-to-file (build-path express_path "report.scrbl") #:exists 'append
      (lambda ()
        (printf "@include-section[\"overview/overview.scrbl\"]\n\n")))

    (make-directory* scrbl_dir)

    (copy-file 
     overview_png
     (build-path scrbl_dir "overview.png"))

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Overview}\n\n")
        (printf "structure of qr code.\n")
        (printf "@section{QR Code Structure}\n")
        (printf "@image{overview/overview.png}")
        ))))
