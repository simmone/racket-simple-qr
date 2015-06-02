#lang racket

(require rackunit/text-ui)

(require rackunit "func.rkt")

(define test-func
  (test-suite 
   "test-func"

   (test-case 
    "test-locate-brick"
    
    (let ([place_pair (locate-brick 1 (cons 1 1))])
      (check-equal? (car place_pair) 1)
      (check-equal? (cdr place_pair) 1))

    (let ([place_pair (locate-brick 2 (cons 1 1))])
      (check-equal? (car place_pair) 2)
      (check-equal? (cdr place_pair) 2))

    (let ([place_pair (locate-brick 2 (cons 2 2))])
      (check-equal? (car place_pair) 4)
      (check-equal? (cdr place_pair) 4))

    (let ([place_pair (locate-brick 2 (cons 5 5))])
      (check-equal? (car place_pair) 10)
      (check-equal? (cdr place_pair) 10))

    (let ([place_pair (locate-brick 3 (cons 5 5))])
      (check-equal? (car place_pair) 15)
      (check-equal? (cdr place_pair) 15))

    (let ([place_pair (locate-brick 3 (cons 5 7))])
      (check-equal? (car place_pair) 21)
      (check-equal? (cdr place_pair) 15))
    )
   
   ))

(run-tests test-func)
