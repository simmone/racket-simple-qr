#lang racket

(require rackunit/text-ui)

(require rackunit "capacity.rkt")

(define test-capacity
  (test-suite 
   "test-capacity"

   (test-case
    "test-capacity-dataq"

    (check-equal? (hash-ref *capacity_table* "K-Q-2") 12)
    (check-equal? (hash-ref *capacity_table* "B-H-15") 220)
    (check-equal? (hash-ref *capacity_table* "N-L-40") 7089)
    )

   ))

(run-tests test-capacity)
