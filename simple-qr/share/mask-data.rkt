#lang racket

(provide (contract-out
          [get-mask-proc (-> exact-nonnegative-integer? procedure?)]
          ))

(define (get-mask-proc mask)
  (let ([mask_hash
         (hash
          0 (lambda (row column) (= (modulo (+ row column) 2) 0))
          1 (lambda (row column) (= (modulo row 2) 0))
          2 (lambda (row column) (= (modulo column 3) 0))
          3 (lambda (row column) (= (modulo (+ row column) 3) 0))
          4 (lambda (row column) (= (modulo (+ (floor (/ row 2)) (floor (/ column 3))) 2) 0))
          5 (lambda (row column) (= (+ (modulo (* row column) 2) (modulo (* row column) 3)) 0))
          6 (lambda (row column) (= (modulo (+ (modulo (* row column) 2) (modulo (* row column) 3)) 2) 0))
          7 (lambda (row column) (= (modulo (+ (modulo (+ row column) 2) (modulo (* row column) 3)) 2) 0))
          )])
    (hash-ref mask_hash mask)))
