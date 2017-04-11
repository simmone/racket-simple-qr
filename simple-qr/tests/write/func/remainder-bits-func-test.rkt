#lang racket

(require rackunit/text-ui)

(require rackunit "../../../write/lib/func/remainder-bits/remainder-bits-func.rkt")

(define test-remainder-bits-func
  (test-suite 
   "test-remainder-bits-func"

   (test-case
    "test-get-remainder-bits"

    (check-equal? (get-remainder-bits 10) 0)
    (check-equal? (get-remainder-bits 20) 3)
    )

   ))

(run-tests test-remainder-bits-func)
