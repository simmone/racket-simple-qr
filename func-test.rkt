#lang racket

(require rackunit/text-ui)

(require rackunit "func.rkt")

(define test-func
  (test-suite 
   "test-func"

   (test-case 
    "test-locate-brick"
    
    (let ([place_pair (locate-brick 1 (cons 1 1))])
      (check-equal? place_pair '(1 . 1)))

    (let ([place_pair (locate-brick 2 (cons 1 1))])
      (check-equal? place_pair '(2 . 2)))

    (let ([place_pair (locate-brick 2 (cons 2 2))])
      (check-equal? place_pair '(4 . 4)))

    (let ([place_pair (locate-brick 2 (cons 5 5))])
      (check-equal? place_pair '(10 . 10)))

    (let ([place_pair (locate-brick 3 (cons 5 5))])
      (check-equal? place_pair '(15 . 15)))

    (let ([place_pair (locate-brick 3 (cons 5 7))])
      (check-equal? place_pair '(21 . 15)))
    )

   (test-case
    "test-locate-finder-pattern"

    (let-values ([(left_top right_top left_bottom)
                  (locate-finder-pattern 50 20)])
      (check-equal? left_top '(0 . 0))
      (check-equal? right_top '(0 . 0))
      (check-equal? left_bottom '(0 . 0)))
    )
   
   ))

(run-tests test-func)
