#lang racket

(require rackunit/text-ui)

(require rackunit "required-bits.rkt")

(define test-required-bits
  (test-suite 
   "test-required-bits"

   (test-case
    "test-required-bits"

    (check-equal? (hash-ref *required_bits_table* "40-Q") 1666)

    )

   ))

(run-tests test-required-bits)
