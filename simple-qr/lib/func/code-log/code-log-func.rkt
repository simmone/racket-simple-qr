#lang racket

(provide (contract-out
     [a->value (-> exact-nonnegative-integer? exact-nonnegative-integer?)]
     [value->a (-> exact-nonnegative-integer? exact-nonnegative-integer?)]
     ))

(require "code-log-dic.rkt")

(define (a->value a)
  (hash-ref *a_value_table* a))

(define (value->a value)
  (hash-ref *value_a_table* value))


