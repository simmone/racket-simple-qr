#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-add-terminator (-> natural? natural? string? path-string? void?)]
          ))

(define (write-report-add-terminator capacity_char_count capacity_bits_width terminator_added_bits express_path)
  (let* ([scrbl_dir (build-path express_path "add-terminator")]
         [scrbl_file (build-path scrbl_dir "add-terminator.scrbl")]
         [img_file (build-path scrbl_dir "add-terminator.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Add Terminator}\n\n")
        (printf "The end of the data in the complete symbol is indicated by a 4 bit terminator 0000, which is omitted or abbreviated if the remaining symbol capacity after the data bit stream is less than 4 bits. The terminator is not a Mode Indicator as such.\n")
        (printf "@section{Capacity}\n")
        (printf "capacity char count: {~a}, capacity bits width: {~a}" capacity_char_count capacity_bits_width)
        (printf "@section{Bits added terminator}\n")
        (printf (display-list 
                 (split-string terminator_added_bits 8)
                 12))
        ))))
