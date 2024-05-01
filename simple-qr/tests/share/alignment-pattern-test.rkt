#lang racket

(require rackunit/text-ui
         rackunit
         "../../share/alignment-pattern.rkt")

(define test-alignment-pattern
  (test-suite 
   "test-alignment-pattern"

   (test-case 
    "test-get-center-point-sets"
    
    (check-equal? (get-center-point-sets '(6 18)) '((6 . 6) (6 . 18) (18 . 6) (18 . 18)))

    (check-equal? (get-center-point-sets '(6 22 38)) '((6 . 6) (6 . 22) (6 . 38) (22 . 6) (22 . 22) (22 . 38) (38 . 6) (38 . 22) (38 . 38)))
    )
   
   ))

(run-tests test-alignment-pattern)
