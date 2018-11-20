#lang racket

(require rackunit/text-ui)

(require rackunit "../../share/func.rkt")

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
   
   (test-case
    "test-get-points"
    
    (let ([matrix '(
                    (0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0)
                    (0 1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1 0)
                    (0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0)
                    )])
      
      (check-equal? (get-points matrix '((6 . 7) (5 . 6) (4 . 5))) '(0 1 1))
      ))

   (test-case 
    "test-locate-brick"
    
    (let ([place_pair (locate-brick 1 (cons 1 1))])
      (check-equal? place_pair '(0 . 0)))

    (let ([place_pair (locate-brick 2 (cons 1 1))])
      (check-equal? place_pair '(0 . 0)))

    (let ([place_pair (locate-brick 2 (cons 2 2))])
      (check-equal? place_pair '(2 . 2)))

    (let ([place_pair (locate-brick 2 (cons 5 5))])
      (check-equal? place_pair '(8 . 8)))

    (let ([place_pair (locate-brick 3 (cons 5 5))])
      (check-equal? place_pair '(12 . 12)))

    (let ([place_pair (locate-brick 3 (cons 5 7))])
      (check-equal? place_pair '(18 . 12)))
    )

   (test-case
    "test-locate-finder-pattern"

    (let ([start_points (locate-finder-pattern 21)])
      (check-equal? (first start_points) '(1 . 1))
      (check-equal? (second start_points) '(1 . 15))
      (check-equal? (third start_points) '(15 . 1)))
    )

   ))

(run-tests test-func)
