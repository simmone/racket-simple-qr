#lang racket

(require "../../../../share/func.rkt")
(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-final-bits (-> natural? list? path-string? void?)]
          ))

(define (write-report-final-bits module_width points_list express_path)
  (let* ([scrbl_dir (build-path express_path "final-bits")]
         [scrbl_file (build-path scrbl_dir "final-bits.scrbl")]
         [img_file (build-path scrbl_dir "final-bits.img")]
         [points_map (points->points_map points_list)]
         [bits_width (length (car points_list))])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Final Bits}\n\n")
        (printf "decoded bits ready to process.\n")
        (printf "@section{QR Width: ~a, Module Width: ~a}\n" bits_width module_width)
        (printf "@section{Bits Image}\n")
        (draw bits_width 1 points_map (make-hash) img_file)
        (printf "@image{final-bits/final-bits.img}")
        (printf "@section{Bits Data}")
        (printf (display-qr-bits bits_width points_map))
        ))))
