 #lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-start (-> natural? path-string? void?)]
          ))

(define (write-report-start modules express_path)
  (let* ([scrbl_dir (build-path express_path "start")]
         [scrbl_file (build-path scrbl_dir "start.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Start}\n\n")
        (printf "blank qr code.\n")
        (printf "@section{Start Blank QR Code}\n")
        (printf (display-qr-bits modules (make-hash)))
        ))))
