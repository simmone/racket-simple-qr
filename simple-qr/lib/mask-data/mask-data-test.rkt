#lang racket

(require rackunit/text-ui)

(require rackunit "mask-data.rkt")

(define test-mask-data
  (test-suite 
   "test-mask-data"

   (test-case
    "test-mask-data"

    (let ([data '(((2 . 3) . "1") ((3 . 4) . "0") ((4 . 5) . "1") ((6 . 7) . "0") ((7 . 7) . "1") ((2 . 4) . "0"))])
      (check-equal? 
       (mask-data data 0)
       '(((2 . 3) . "1") ((3 . 4) . "0") ((4 . 5) . "1") ((6 . 7) . "0") ((7 . 7) . "0") ((2 . 4) . "1")))

      (check-equal? 
       (mask-data data 1)
       '(((2 . 3) . "0") ((3 . 4) . "0") ((4 . 5) . "0") ((6 . 7) . "1") ((7 . 7) . "1") ((2 . 4) . "1")))

      (check-equal? 
       (mask-data data 2)
       '(((2 . 3) . "0") ((3 . 4) . "0") ((4 . 5) . "1") ((6 . 7) . "0") ((7 . 7) . "1") ((2 . 4) . "0")))

      (check-equal? 
       (mask-data data 3)
       '(((2 . 3) . "1") ((3 . 4) . "0") ((4 . 5) . "0") ((6 . 7) . "0") ((7 . 7) . "1") ((2 . 4) . "1")))

      (check-equal? 
       (mask-data data 4)
       '(((2 . 3) . "0") ((3 . 4) . "1") ((4 . 5) . "1") ((6 . 7) . "0") ((7 . 7) . "1") ((2 . 4) . "1")))

      (check-equal? 
       (mask-data data 5)
       '(((2 . 3) . "0") ((3 . 4) . "1") ((4 . 5) . "1") ((6 . 7) . "1") ((7 . 7) . "1") ((2 . 4) . "0")))

      (check-equal? 
       (mask-data data 6)
       '(((2 . 3) . "0") ((3 . 4) . "1") ((4 . 5) . "0") ((6 . 7) . "1") ((7 . 7) . "0") ((2 . 4) . "1")))

      (check-equal? 
       (mask-data data 7)
       '(((2 . 3) . "1") ((3 . 4) . "0") ((4 . 5) . "1") ((6 . 7) . "0") ((7 . 7) . "1") ((2 . 4) . "1")))
      )

    )
   
    ))

  (run-tests test-mask-data)
