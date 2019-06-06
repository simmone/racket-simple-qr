#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-data-bits (-> string? string? path-string? void?)]
          ))

(define (write-report-data-bits data data_bits express_path)
  (let* ([scrbl_dir (build-path express_path "data-bits")]
         [scrbl_file (build-path scrbl_dir "data-bits.scrbl")]
         [img_file (build-path scrbl_dir "data-bits.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Data Bits}\n\n")
        (printf "transform data string to bits\n")
        (printf "@section{Data Table}\n")
        (printf (display-double-list 
                 (string->list data)
                 (split-string data_bits 8)
                 12))
        ))))
