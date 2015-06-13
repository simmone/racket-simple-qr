#lang racket

(require rackunit/text-ui)

(require rackunit "character-count.rkt")

(define test-character-count
  (test-suite 
   "test-character-count"

   (test-case
    "test-get-character-count"

    (check-equal? (get-character-count 1 "N") 10)
    (check-equal? (get-character-count 1 "A") 9)
    (check-equal? (get-character-count 1 "B") 8)
    (check-equal? (get-character-count 1 "K") 8)

    (check-equal? (get-character-count 10 "N") 12)
    (check-equal? (get-character-count 26 "A") 11)

    (check-equal? (get-character-count 27 "N") 14)
    (check-equal? (get-character-count 40 "A") 13)
    )

   ))

(run-tests test-character-count)
