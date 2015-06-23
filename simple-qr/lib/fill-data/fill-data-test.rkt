#lang racket

(require rackunit/text-ui)

(require rackunit "fill-data.rkt")

(define test-fill-data
  (test-suite 
   "test-fill-data"

   (test-case
    "test-get-snake-modules"

    (check-equal? (snake-modules 6)
                  '((6 . 6) (6 . 5) (5 . 6) (5 . 5) (4 . 6) (4 . 5) (3 . 6) (3 . 5) (2 . 6) (2 . 5) (1 . 6) (1 . 5)
                    (1 . 4) (1 . 3) (2 . 4) (2 . 3) (3 . 4) (3 . 3) (4 . 4) (4 . 3) (5 . 4) (5 . 3) (6 . 4) (6 . 3)
                    (6 . 2) (6 . 1) (5 . 2) (5 . 1) (4 . 2) (4 . 1) (3 . 2) (3 . 1) (2 . 2) (2 . 1) (1 . 2) (1 . 1)))

;    (check-equal? (snake-modules 5)
;                  '((5 . 5) (5 . 4) (4 . 5) (4 . 4) (3 . 5) (3 . 4) (2 . 5) (2 . 4) (1 . 5) (1 . 4)
;                    (1 . 3) (1 . 2) (2 . 3) (2 . 2) (3 . 3) (3 . 2) (4 . 3) (4 . 2) (5 . 3) (5 . 2)
;                    (5 . 1) (4 . 1) (3 . 1) (2 . 1) (1 . 1)))
    )
   ))

(run-tests test-fill-data)
