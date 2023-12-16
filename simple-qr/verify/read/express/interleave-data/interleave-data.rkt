#lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-interleave-data (-> list? natural? list? list? list? list? string? list? list? list? string? path-string? void?)]
          ))

(define (write-report-interleave-data 
         group_width
         ec_count
         bit8_list
         data_count_list
         data_bit8_list
         data_sequence
         un_interleave_data_bits
         ec_count_list
         ec_bit8_list
         ec_sequence
         un_interleave_ec_bits
         express_path)

  (let* ([scrbl_dir (build-path express_path "interleave-data")]
         [scrbl_file (build-path scrbl_dir "interleave-data.scrbl")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Interleave Data}\n\n")
        (printf "interleave data bits.\n")
        (printf "@section{Group Define: ~a}\n" group_width)
        (printf "@section{Ec Count(1): ~a}\n" ec_count)

        (printf "@section{Data Count List: ~a}\n" data_count_list)
        (printf "@section{Interleaved Data}\n")
        (printf (display-list data_bit8_list 13 10))
        (printf "@section{Data Sequence}\n")
        (printf (display-list data_sequence))
        (printf "@section{UnInterleaved Data}\n")
        (printf (display-double-list 
                 (map
                  (lambda (bit8)
                    (string->number bit8 2))
                  (split-string un_interleave_data_bits 8))
                 (split-string un_interleave_data_bits 8)
                 12))

        (printf "@section{Ec Count List: ~a}\n" ec_count_list)
        (printf "@section{Interleaved Ec}\n")
        (printf (display-list ec_bit8_list 13 10))
        (printf "@section{Ec Sequence}\n")
        (printf (display-list ec_sequence))
        (printf "@section{UnInterleaved Ec}\n")
        (printf (display-double-list 
                 (map
                  (lambda (bit8)
                    (string->number bit8 2))
                  (split-string un_interleave_ec_bits 8))
                 (split-string un_interleave_ec_bits 8)
                 12))
        ))))
