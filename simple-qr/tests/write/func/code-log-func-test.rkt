#lang racket

(require rackunit/text-ui)

(require rackunit "../../../write/lib/func/code-log/code-log-func.rkt")

(define test-code-log-func
  (test-suite 
   "test-code-log-func"

   (test-case
    "test-a->value"

    (check-equal? (a->value 40) 106)
    )

   (test-case
    "test-value->a"

    (check-equal? (value->a 106) 40)
    )

   ))

(run-tests test-code-log-func)
