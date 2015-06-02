#lang racket

(require rackunit/text-ui)

(require rackunit "func.rkt")

(define test-func
  (test-suite 
   "test-func"

   (test-case 
    "test-locate-brick"

    (let-values ([(x y) (locate-brick #:matrix 1 #:brick_width 1 #:row 1 #:col 1)])
      (check-equal? x 0)
      (check-equal? y 0))

    (let-values ([(x y) (locate-brick #:matrix 5 #:brick_width 2 #:row 1 #:col 1)])
      (check-equal? x 0)
      (check-equal? y 0))

    (let-values ([(x y) (locate-brick #:matrix 5 #:brick_width 2 #:row 2 #:col 2)])
      (check-equal? x 2)
      (check-equal? y 2))

    (let-values ([(x y) (locate-brick #:matrix 5 #:brick_width 2 #:row 5 #:col 5)])
      (check-equal? x 8)
      (check-equal? y 8))

    )
   
   ))

(run-tests test-func)
