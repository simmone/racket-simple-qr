#lang racket

(provide (contract-out
          [get-dark-point (-> exact-nonnegative-integer? pair?)]
          ))

(define (get-dark-point version)
  (cons (+ (* 4 version) 10) 9))
