#lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-ungroup-data (-> list? list? list? list? string? path-string? void?)]
          ))

(define (write-report-ungroup-data group_list count_list data_groups sequence ungroup_bits express_path)
  (let* ([scrbl_dir (build-path express_path "ungroup-data")]
         [scrbl_file (build-path scrbl_dir "ungroup-data.scrbl")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Ungroup Data}\n\n")
        (printf "ungroup data bits.\n")
        (printf "@section{Group Define: ~a}\n" group_list)
        (printf "@section{Count List: ~a}\n" count_list)
        (printf "@section{Data Groups}\n")
        (printf (display-list data_groups))
        (printf "@section{Sequence}\n")
        (printf (display-list sequence))
        (printf "@section{Decimal List}\n")
        (printf (display-double-list 
                 (map
                  (lambda (bit8)
                    (string->number bit8 2))
                  (split-string ungroup_bits 8))
                 (split-string ungroup_bits 8)
                 12))
        ))))
