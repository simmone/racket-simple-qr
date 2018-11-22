#lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-decimal-list (-> string? list? path-string? void?)]
          ))

(define (write-report-decimal-list data_bits decimal_list express_path)
  (let* ([scrbl_dir (build-path express_path "decimal-list")]
         [scrbl_file (build-path scrbl_dir "decimal-list.scrbl")]
         [img_file (build-path scrbl_dir "decimal-list.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{To Decimal List}\n\n")
        (printf "turn bits to decimal list.")
        (printf "@section{Decimal List}\n")
        (printf (display-double-list
                 decimal_list
                 (split-string data_bits 8)
                 12))
        ))))
