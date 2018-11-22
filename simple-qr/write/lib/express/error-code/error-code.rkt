#lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-error-code (-> natural? string? list? path-string? void?)]
          ))

(define (write-report-error-code ec_count origin_poly_generator error_code_group express_path)
  (let* ([scrbl_dir (build-path express_path "error-code")]
         [scrbl_file (build-path scrbl_dir "error-code.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Error Code}\n\n")
        (printf "calculate error code on each group.\n")
        (printf "@section{Start Calculate}\n")
        (printf "ec count: ~a\norigin_poly_generator: @verbatim{~a}\n" ec_count origin_poly_generator)
        (printf "@section{Error Code Group 1}\n")
        (for-each
         (lambda (data_list)
           (printf "group data:\n")
           (printf (display-list (first data_list) 4))

           (printf "error code:\n")
           (printf (display-list (second data_list) 4)))
         (first error_code_group))
        (printf "@section{Error Code Group 2}\n")
        (for-each
         (lambda (data_list)
           (printf "group data:\n")
           (printf (display-list (first data_list) 4))

           (printf "error code:\n")
           (printf (display-list (second data_list) 4)))
         (second error_code_group))
        ))))
