#lang racket

(require rackunit/text-ui)

(require rackunit "../../share/character-bit-width.rkt")

(define test-character-bit-width
  (test-suite 
   "test-character-bit-width"

   (test-case
    "test-get-character-bit-width"

    (check-equal? (get-character-bit-width 1 "N") 10)
    (check-equal? (get-character-bit-width 1 "A") 9)
    (check-equal? (get-character-bit-width 1 "B") 8)
    (check-equal? (get-character-bit-width 1 "K") 8)

    (check-equal? (get-character-bit-width 10 "N") 12)
    (check-equal? (get-character-bit-width 26 "A") 11)

    (check-equal? (get-character-bit-width 27 "N") 14)
    (check-equal? (get-character-bit-width 40 "A") 13)
    )

   ))

(run-tests test-character-bit-width)
