#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-interleave-data-group (-> list? string? path-string? void?)]
          ))

(define (write-report-interleave-data-group interleave_data_group interleave_data_bits express_path)
  (let* ([scrbl_dir (build-path express_path "interleave-data-group")]
         [scrbl_file (build-path scrbl_dir "interleave-data-group.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Interleave Data Group}\n\n")
        (printf "interleave data group.\n")
        (printf "@section{Interleaved Data}\n")
        (printf (display-double-list 
                 interleave_data_group
                 (split-string interleave_data_bits 8)
                 12))
        ))))
