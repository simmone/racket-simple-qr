#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-group-decimal-list (-> list? list? path-string? void?)]
          ))

(define (write-report-group-decimal-list contract_list group_list express_path)
  (let* ([scrbl_dir (build-path express_path "group-decimal-list")]
         [scrbl_file (build-path scrbl_dir "group-decimal-list.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Split Two Group}\n\n")
        (printf "split decimal list into two lists: @verbatim{((group1-rows . group1-cols) (group2-rows . group2-cols))}")
        (printf "@section{Group Contract}\n")
        (printf "~a" contract_list)
        (printf "@section{Group 1}\n")
        (for-each
         (lambda (data_list)
           (printf (display-list data_list 4)))
         (car group_list))
        (printf "@section{Group 2}\n")
        (for-each
         (lambda (data_list)
           (printf (display-list data_list 4)))
         (cdr group_list))
        ))))
