#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-repeat-pad (-> natural? string? string? path-string? void?)]
          ))

(define (write-report-repeat-pad capacity_bits_width repeat_str repeat_padded_bits express_path)
  (let* ([scrbl_dir (build-path express_path "repeat-pad")]
         [scrbl_file (build-path scrbl_dir "repeat-pad.scrbl")]
         [img_file (build-path scrbl_dir "repeat-pad.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Repeat Pad}\n\n")
        (printf "repeat pad a fix bit string to reach capacity.")
        (printf "@section{Report String}\n")
        (printf "capacity bits width:~a, capacity char count: ~a, crepeat string: ~a\n" capacity_bits_width (/ capacity_bits_width 8) repeat_str)
        (printf "@section{Repeat Padded Bits}\n")
        (printf (display-list 
                 (split-string repeat_padded_bits 8)
                 12))
        ))))
