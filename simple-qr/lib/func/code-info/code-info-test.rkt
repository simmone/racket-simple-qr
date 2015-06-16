#lang racket

(require rackunit/text-ui)

(require rackunit "code-info.rkt")

(define test-code-info
  (test-suite 
   "test-code-info"

   (test-case
    "test-code-info"

    (check-equal? (hash-ref *required_bits_table* "40-Q") 1666)

    (check-equal? (hash-ref *required_ec_table* "40-Q") 30)

    (check-equal? (hash-ref *required_group_table* "40-Q") #((34 . 24) (34 . 25)))
    )

   ))

(run-tests test-code-info)
