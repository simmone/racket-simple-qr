#lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-append-remainder (-> natural? natural? string? path-string? void?)]
          ))

(define (write-report-append-remainder s18_length remainder_bits_width data_bits express_path)
  (let* ([scrbl_dir (build-path express_path "append-remainder")]
         [scrbl_file (build-path scrbl_dir "append-remainder.scrbl")])

    (with-output-to-file (build-path express_path "report.scrbl") #:exists 'append
      (lambda ()
        (printf "@include-section[\"append-remainder/append-remainder.scrbl\"]\n\n")))

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Append Remainder Bits}\n\n")
        (printf "append remainder bits.\n")
        (printf "@section{Remainder Added Data}\n")
        (printf "total length = (+ s18_length:~a s19_remainder_bits_width:~a) = ~a\n\n"
                s18_length remainder_bits_width (+ s18_length remainder_bits_width))
        (printf (display-list (split-string data_bits 8) 12))
        ))))
