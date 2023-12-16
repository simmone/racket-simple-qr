#lang racket

(require "../../../../share/func.rkt")
(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-trimed-bits (-> natural? list? path-string? void?)]
          ))

(define (write-report-trimed-bits bits_width points_list express_path)
  (let* ([scrbl_dir (build-path express_path "trimed-bits")]
         [scrbl_file (build-path scrbl_dir "trimed-bits.scrbl")]
         [img_file (build-path scrbl_dir "trimed-bits.img")]
         [points_map (points->points_map points_list)])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Trimed}\n\n")
        (printf "trimed outer white space.\n")
        (printf "@section{Trimed Bits}\n")
        (draw bits_width 1 points_map (make-hash) img_file)
        (printf "@image{trimed-bits/trimed-bits.img}")
        ))))
