#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-append-remainder (-> natural? string? path-string? void?)]
          ))

(define (write-report-append-remainder remainder_bits_width data_bits express_path)
  (let* ([scrbl_dir (build-path express_path "append-remainder")]
         [scrbl_file (build-path scrbl_dir "append-remainder.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Append Remainder Bits}\n\n")
        (printf "append remainder bits.\n")
        (printf "@section{Remainder Added Data}\n")
        (printf "remainder bits width: ~a\n" remainder_bits_width)
        (printf (display-list (split-string data_bits 8) 12))
        ))))
