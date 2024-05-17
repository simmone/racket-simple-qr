#lang racket

(require rackunit/text-ui
         rackunit
         "../../write/timing-pattern.rkt")

(define test-timing-pattern
  (test-suite
   "test-timing-pattern"

   (test-case
    "test-locate-timing-pattern-joints"

    (let ([joints (locate-timing-pattern-joints 21)])
      (check-equal? (second joints) '((8 . 6) (12 . 6)))
      (check-equal? (first joints) '((6 . 8) (6 . 12))))
    )

   ))

(run-tests test-timing-pattern)
