#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-bw-bits (-> natural? natural? list? path-string? void?)]
          ))

(define (write-report-bw-bits threshold bits_width points_list express_path)
  (let* ([scrbl_dir (build-path express_path "bw-bits")]
         [scrbl_file (build-path scrbl_dir "bw-bits.scrbl")]
         [img_file (build-path scrbl_dir "bw-bits.img")]
         [points_map (points->points_map points_list)])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Color Bits to 0|1}\n\n")
        (printf "turn pixel's color to 0 or 1 depends on threshold.\n")
        (printf "@section{threshold:~a}\n" threshold)
        (printf "color deeper than threshold, it's 1, or it's 0.\n")
        (printf "@section{Bw Bits}\n")
        (printf (display-qr-bits bits_width points_map))
        (printf "@section{Bw Image}\n")
        (draw bits_width 1 points_map (make-hash) img_file)
        (printf "@image{bw-bits/bw-bits.img}")
        ))))
