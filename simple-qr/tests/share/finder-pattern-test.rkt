#lang racket

(require rackunit/text-ui)

(require rackunit "../../share/finder-pattern.rkt")

(define test-finder-pattern
  (test-suite 
   "test-finder-pattern"

   (test-case
    "test-locate-finder-pattern"

    (let ([start_points (locate-finder-pattern 21)])
      (check-equal? (first start_points) '(1 . 1))
      (check-equal? (second start_points) '(1 . 15))
      (check-equal? (third start_points) '(15 . 1)))
    )
   ))

(run-tests test-finder-pattern)
