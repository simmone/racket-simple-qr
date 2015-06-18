#lang racket

(require racket/string)

(with-output-to-file 
 "code-log-dic.rkt" #:exists 'replace
 (lambda ()
   (let-values ([(a-value-list)
                 (with-input-from-file "code_log_table.txt"
                   (lambda ()
                     (let loop ([line (string-trim (read-line))]
                                [a-gf-list '()])
                       (if (not (eof-object? line))
                           (let ([items (regexp-split #rx"\t" line)])
                             (loop (read-line) 
                                   (cons (cons (list-ref items 0) (list-ref items 1)) a-gf-list)))
                           (values (reverse a-gf-list))))))])
     (printf "#lang racket\n\n")
     (printf "(provide (contract-out\n")
     (printf "     [*a_value_table* hash?]\n")
     (printf "     [*value_a_table* hash?]\n")
     (printf "     ))\n\n")

     (printf "(define *a_value_table*\n")
     (printf "  '#hash(\n")
     (for-each
      (lambda (item)
        (printf "         ~a\n" item))
      a-value-list)
     (printf "         ))\n\n")

     (printf "(define *value_a_table*\n")
     (printf "  '#hash(\n")
     (for-each
      (lambda (item)
        (printf "         (~a . ~a)\n" (cdr item) (car item)))
      a-value-list)
     (printf "         ))\n")

     )))
                 
                 
                 
