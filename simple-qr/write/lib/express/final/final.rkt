#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-final (-> hash? natural? path-string? void?)]
          ))

(define (write-report-final points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "final")]
         [scrbl_file (build-path scrbl_dir "final.scrbl")]
         [img_file (build-path scrbl_dir "final.img")])

    (with-output-to-file (build-path express_path "report.scrbl") #:exists 'append
      (lambda ()
        (printf "@include-section[\"final/final.scrbl\"]\n\n")))

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{QR Image}\n\n")
        (printf "final image and bits.\n")
        (printf "@section{Final Bits}\n")
        (printf (display-qr-bits modules points_map))
        (printf "@section{Final Image}\n")
        (draw modules 10 points_map (make-hash) '("black" . "white") img_file)
        (printf "@image{final/final.img}")
        ))))
