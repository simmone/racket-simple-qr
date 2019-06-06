#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-fill-data (-> hash? natural? path-string? void?)]
          ))

(define (write-report-fill-data points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "fill-data")]
         [scrbl_file (build-path scrbl_dir "fill-data.scrbl")]
         [img_file (build-path scrbl_dir "fill-data.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Fill Data}\n\n")
        (printf "draw data.\n")
        (printf "@section{Fill Data Bits}\n")
        (printf (display-qr-bits modules points_map))
        (printf "@section{Fill Data Image}\n")
        (draw modules 5 points_map (make-hash) img_file)
        (printf "@image{fill-data/fill-data.img}")
        ))))
