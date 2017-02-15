#lang racket

(require rackunit/text-ui)

(require rackunit "data-group.rkt")

(define test-data-group
  (test-suite 
   "test-data-group"

   (test-case
    "test-defines->count_list"

    (check-equal? 
     (defines->count-list #((1 . 19) (0 . 0)))
     '(19))

    (check-equal? 
     (defines->count-list #((2 . 13) (0 . 0)))
     '(13 13))

    (check-equal? 
     (defines->count-list #((4 . 9) (0 . 0)))
     '(9 9 9 9))

    (check-equal? 
     (defines->count-list #((2 . 15) (2 . 16)))
     '(15 15 16 16))
    )

    ))

  (run-tests test-data-group)
