#lang racket

(require rackunit/text-ui)

(require rackunit "func.rkt")

(define test-func
  (test-suite 
   "test-func"

   (test-case
    "test-get-points-between"

    (check-equal?
     (get-points-between '(1 . 1) '(1 . 10) #:direction 'horizontal)
     '((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6) (1 . 7) (1 . 8) (1 . 9) (1 . 10)))

    (check-equal?
     (get-points-between '(1 . 1) '(1 . 10) #:direction 'vertical)
     '())

    (check-equal?
     (get-points-between '(1 . 1) '(5 . 1) #:direction 'vertical)
     '((1 . 1) (2 . 1) (3 . 1) (4 . 1) (5 . 1)))

    (check-equal?
     (get-points-between '(1 . 1) '(5 . 1) #:direction 'horizontal)
     '())

    (check-equal?
     (get-points-between '(1 . 1) '(5 . 2) #:direction 'vertical)
     '())
    
    )

   ))

(run-tests test-func)
