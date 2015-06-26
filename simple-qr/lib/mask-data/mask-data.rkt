#lang racket

(provide (contract-out
          [mask-data (-> list? exact-nonnegative-integer? list?)]
          ))

(define *mask_proc_hash*
  (hash
   0 (lambda (row column) (= (modulo (+ row column) 2) 0))
   1 (lambda (row column) (= (modulo row 2) 0))
   2 (lambda (row column) (= (modulo column 3) 0))
   3 (lambda (row column) (= (modulo (+ row column) 3) 0))
   4 (lambda (row column) (= (modulo (+ (floor (/ row 2)) (floor (/ column 3))) 2) 0))
   5 (lambda (row column) (= (+ (modulo (* row column) 2) (modulo (* row column) 3)) 0))
   6 (lambda (row column) (= (modulo (+ (modulo (* row column) 2) (modulo (* row column) 3)) 2) 0))
   7 (lambda (row column) (= (modulo (+ (modulo (+ row column) 2) (modulo (* row column) 3)) 2) 0))
   ))

(define (mask-data data_list mask_number)
  (let ([mask-func (hash-ref *mask_proc_hash* mask_number)])
    (reverse
     (let loop ([loop_list data_list]
                [result_list '()])
       (if (not (null? loop_list))
           (let ([point_pair (caar loop_list)]
                 [bit (cdar loop_list)])
             (loop
              (cdr loop_list)
              (cons (cons point_pair (if (mask-func (car point_pair) (cdr point_pair)) (switch-bit bit) bit)) result_list)))
           result_list)))))

(define (switch-bit bit)
  (if (string=? bit "1")
      "0"
      "1"))
          
