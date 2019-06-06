#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-separator (-> hash? natural? path-string? void?)]
          ))

(define (write-report-separator points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "separator")]
         [scrbl_file (build-path scrbl_dir "separator.scrbl")]
         [img_file (build-path scrbl_dir "separator.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Separator}\n\n")
        (printf "draw separator.\n")
        (printf "@section{Separator Bits}\n")
        (printf (display-qr-bits modules points_map))
        (printf "@section{Separator Image}\n")
        (draw modules 5 points_map (make-hash) img_file)
        (printf "@image{separator/separator.img}")
        ))))
