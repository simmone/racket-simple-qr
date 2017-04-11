#lang racket

(provide (contract-out
     [get-remainder-bits (-> exact-nonnegative-integer? exact-nonnegative-integer?)]
     ))

(require "remainder-bits-dic.rkt")

(define (get-remainder-bits version)
  (hash-ref *remainder_bits_table* version))
