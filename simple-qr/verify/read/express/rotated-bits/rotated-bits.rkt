#lang racket

(require "../../../../share/func.rkt")
(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-rotated-bits (-> natural? list? path-string? void?)]
          ))

(define (write-report-rotated-bits bits_width points_list express_path)
  (let* ([scrbl_dir (build-path express_path "rotated-bits")]
         [scrbl_file (build-path scrbl_dir "rotated-bits.scrbl")]
         [img_file (build-path scrbl_dir "rotated-bits.img")]
         [points_map (points->points_map points_list)])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Rotated}\n\n")
        (printf "rotate image depends on rotate ratio.\n")
        (printf "@section{Rotated Bits}\n")
        (draw bits_width 1 points_map (make-hash) img_file)
        (printf "@image{rotated-bits/rotated-bits.img}")
        ))))
