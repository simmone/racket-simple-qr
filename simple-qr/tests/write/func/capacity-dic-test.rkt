#lang racket

(require rackunit/text-ui)

(require rackunit "../../../write/lib/func/capacity/capacity-dic.rkt")

(define test-capacity
  (test-suite 
   "test-capacity"

   (test-case
    "test-capacity-dataq"

    (check-equal? (list-ref (hash-ref *capacity_table* "K-Q") 7) '(8 . 66))
    (check-equal? (list-ref (hash-ref *capacity_table* "B-M") 29) '(30 . 1370))
    (check-equal? (list-ref (hash-ref *capacity_table* "A-L") 38) '(39 . 4087))
    (check-equal? (list-ref (hash-ref *capacity_table* "A-L") 39) '(40 . 4296))

    )

   ))

(run-tests test-capacity)
