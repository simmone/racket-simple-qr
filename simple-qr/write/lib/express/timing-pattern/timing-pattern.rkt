#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")


(provide (contract-out
          [write-report-timing-pattern (-> hash? natural? path-string? void?)]
          ))

(define (write-report-timing-pattern points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "timing-pattern")]
         [scrbl_file (build-path scrbl_dir "timing-pattern.scrbl")]
         [img_file (build-path scrbl_dir "timing-pattern.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Timing Pattern}\n\n")
        (printf "draw timing pattern.\n")
        (printf "@section{Timing Pattern Bits}\n")
        (printf (display-qr-bits modules points_map))
        (printf "@section{Timing Pattern Image}\n")
        (draw modules 5 points_map (make-hash) img_file)
        (printf "@image{timing-pattern/timing-pattern.img}")
        ))))
