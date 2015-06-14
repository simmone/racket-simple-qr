#lang racket

(require rackunit/text-ui)

(require rackunit "capacity-func.rkt")

(define test-capacity
  (test-suite 
   "test-capacity"

   (test-case
    "test-get-version-origin"

    (check-equal? (get-version-origin 10 "B" "Q") 1)
    (check-equal? (get-version-origin 1663 "B" "Q") 40)
    (check-equal? (get-version-origin 30 "B" "L") 2)
    (check-equal? (get-version-origin 30 "B" "H") 4)
    (check-equal? (get-version-origin 100 "A" "L") 4)
    (check-equal? (get-version-origin 2100 "B" "L") 34)
    (check-equal? (get-version-origin 7089 "N" "L") 40)
    (check-exn exn:fail? (lambda () (get-version-origin 1664 "B" "Q")))
    )

   ))

(run-tests test-capacity)
