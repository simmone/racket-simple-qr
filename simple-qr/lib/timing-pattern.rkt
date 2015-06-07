#lang racket

(require "func.rkt")

(provide (contract-out
          [locate-timing-pattern-joints (-> 
                                      exact-nonnegative-integer?
                                      list?)]
          ))

(define (locate-timing-pattern-joints modules)
  (let ([joint (+ 9 (- modules 16 1))])
    (list (cons (cons 9  7) (cons joint 7))
          (cons (cons 7  9) (cons 7 joint)))))

