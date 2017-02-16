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

   (test-case
    "test-count_list->sequence_list"
    
    (check-equal?
     (count_list->sequence_list '(19))
     '((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)))

    (check-equal?
     (count_list->sequence_list '(13 13))
     '(
       (0  1  2  3  4  5  6  7  8  9  10 11 12)
       (13 14 15 16 17 18 19 20 21 22 23 24 25)
       ))

    (check-equal?
     (count_list->sequence_list '(9 9 9 9))
     '(
       (0  1  2  3  4  5  6  7  8)
       (9  10 11 12 13 14 15 16 17)
       (18 19 20 21 22 23 24 25 26)
       (27 28 29 30 31 32 33 34 35)
       ))

    (check-equal?
     (count_list->sequence_list '(15 15 16 16))
     '(
       (0  1  2  3  4  5  6  7  8  9  10 11 12 13 14)
       (15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)
       (30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45)
       (46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61)
       ))
    )

    ))

  (run-tests test-data-group)
