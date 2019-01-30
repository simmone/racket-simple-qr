#lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-data-recovery (-> natural? list? list? list? string? path-string? void?)]
          ))

(define (write-report-data-recovery ec_count count_list raw_data_groups recovered_data_groups data_bits express_path)
  (let* ([scrbl_dir (build-path express_path "data-recovery")]
         [scrbl_file (build-path scrbl_dir "data-recovery.scrbl")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Data Recovery}\n\n")
        (printf "use Reed-Solomon algorithm to recover group data.\n")
        (printf "@section{Ec Count(2): ~a}\n" ec_count)
        (printf "@section{Group Data Width: ~a}\n" count_list)
        (printf "@section{Raw Data Groups}\n")
        (for-each
         (lambda (line)
           (printf (display-list line)))
         raw_data_groups)
        (printf "@section{Recovered Data Groups}\n")
        (for-each
         (lambda (line)
           (printf (display-list line)))
         recovered_data_groups)
        (printf "@section{Data Bits}\n")
        (printf (display-double-list 
                 (map
                  (lambda (bit8)
                    (string->number bit8 2))
                  (split-string data_bits 8))
                 (split-string data_bits 8)
                 12))
        ))))
