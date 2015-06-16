#lang racket

(require rackunit/text-ui)

(require rackunit "code-info-func.rkt")

(define test-code-info-func
  (test-suite 
   "test-code-info-func"

   (test-case
    "test-get-bits-width"

    (check-equal? (get-bits-width 40 "Q") 1666)
    )

   (test-case
    "test-get-ec-count"

    (check-equal? (get-ec-count 40 "Q") 30)
    )

   (test-case
    "test-get-group-width"

    (check-equal? (get-group-width 40 "Q") #((34 . 24) (34 . 25)))
    )

   ))

(run-tests test-code-info-func)
