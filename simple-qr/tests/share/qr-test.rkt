#lang racket

(require rackunit/text-ui)

(require rackunit "../../share/qr.rkt")

(define test-qr
  (test-suite 
   "test-qr"

   (test-case
    "test-version->modules"

    (check-equal? (version->modules 1) 21)
    (check-equal? (version->modules 2) 25)
    (check-equal? (version->modules 3) 29)
    (check-equal? (version->modules 5) 37)
    (check-equal? (version->modules 39) 173)    
    (check-equal? (version->modules 40) 177)
    )
   ))

(run-tests test-qr)