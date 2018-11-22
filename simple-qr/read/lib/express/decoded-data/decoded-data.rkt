#lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-decoded-data (-> string? natural? bytes? list? path-string? void?)]
          ))

(define (write-report-decoded-data mode data_count bytes_list bit8_list express_path)
  (let* ([scrbl_dir (build-path express_path "decoded-data")]
         [scrbl_file (build-path scrbl_dir "decoded-data.scrbl")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Decoded Data}\n\n")
        (printf "decoded data list.\n")
        (printf "@section{Mode: ~a}\n" mode)
        (printf "@section{Data Count: ~a}\n" data_count)
        (printf "@section{Final Data List}\n")
        (printf (display-double-list
                 (map (lambda (bt) (integer->char bt)) (bytes->list bytes_list))
                 bit8_list
                 12))
        ))))
