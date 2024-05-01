#lang racket

(require rackunit/text-ui
         rackunit
         "../../share/data-group.rkt")

(define test-data-group
  (test-suite 
   "test-data-group"

   (test-case
    "test-defines->count_list"

    (check-equal? 
     (defines->count-list '((1 . 19) (0 . 0)))
     '(19))

    (check-equal? 
     (defines->count-list '((2 . 13) (0 . 0)))
     '(13 13))

    (check-equal? 
     (defines->count-list '((4 . 9) (0 . 0)))
     '(9 9 9 9))

    (check-equal? 
     (defines->count-list '((2 . 15) (2 . 16)))
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

   (test-case
    "test-sequence_list->sequence"
    
    (check-equal?
     (sequence_list->sequence
      '((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))
      )
     '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))

    (check-equal?
     (sequence_list->sequence
     '(
       (0  1  2  3  4  5  6  7  8  9  10 11 12)
       (13 14 15 16 17 18 19 20 21 22 23 24 25)
       ))
     '(0 13 1 14 2 15 3 16 4 17 5 18 6 19 7 20 8 21 9 22 10 23 11 24 12 25))

    (check-equal?
     (sequence_list->sequence
     '(
       (0  1  2  3  4  5  6  7  8)
       (9  10 11 12 13 14 15 16 17)
       (18 19 20 21 22 23 24 25 26)
       (27 28 29 30 31 32 33 34 35)
       ))
     '(0 9 18 27
       1 10 19 28
       2 11 20 29
       3 12 21 30
       4 13 22 31
       5 14 23 32
       6 15 24 33
       7 16 25 34
       8 17 26 35)
     )

    (check-equal?
     (sequence_list->sequence
     '(
       (0  1  2  3  4  5  6  7  8  9  10 11 12 13 14)
       (15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)
       (30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45)
       (46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61)
       ))
     '(0 15 30 46
       1 16 31 47
       2 17 32 48
       3 18 33 49
       4 19 34 50
       5 20 35 51
       6 21 36 52
       7 22 37 53
       8 23 38 54
       9 24 39 55
       10 25 40 56
       11 26 41 57
       12 27 42 58
       13 28 43 59
       14 29 44 60
       45 61))

    (check-equal?
     (sequence_list->sequence
     '(
       (0  1)
       (3 4 5)
       (6 7 8 9)
       (10 11 12 13 14 15)))
     '(0 3 6 10 1 4 7 11 5 8 12 9 13 14 15))
    )

   (test-case
    "test-bit8->list"
    
    (check-equal?
     (bit8->list
      (string-append
       "01000001"
       "00100110"
       "01010110"
       "10100110"
       "10000111"
       "10000111"
       "0100011100")
      4)
      (list
       "01000001"
       "00100110"
       "01010110"
       "10100110")))
   
   (test-case
    "test-combine-data-sequence"
    
    (check-equal?
     (combine-data-sequence
      '(
        "01000001"
        "00100110"
        "01010110"
        "01000001"
        "00100110"
        "01010110"
        "01000001"
        "00100110"
        "01010110"
        "01010110")
      '(0 3 6 10 1 4 7 11 5 8))

      '(
        ("01000001" . 0)
        ("00100110" . 3)
        ("01010110" . 6)
        ("01000001" . 10)
        ("00100110" . 1)
        ("01010110" . 4)
        ("01000001" . 7)
        ("00100110" . 11)
        ("01010110" . 5)
        ("01010110" . 8))))
   
    ))

  (run-tests test-data-group)
