#lang racket

(require rackunit/text-ui)

(require rackunit "error-correct-code.rkt")

(define test-error-correct-code
  (test-suite
   "test-error-correct-code"

   (test-case
    "test-to-message-poly"

    (check-equal? (to-message-poly '(32, 91, 11, 120, 209, 114, 220, 77, 67, 64, 236, 17, 236, 17, 236, 17))
                  "a5x15+a92x14+11x13+120x12+209x11+114x10+220x9+77x8+67x7+64x6+236x5+17x4+236x3+17x2+236x1+17x0")
    )

   ))

(run-tests test-error-correct-code)
