#lang racket

(require rackunit/text-ui)

(require rackunit "error-correct-code.rkt")

(define test-error-correct-code
  (test-suite
   "test-error-correct-code"

   (test-case
    "test-to-message-poly"

    (check-equal? (to-message-poly '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17))
                  "a5x15+a92x14+a238x13+a78x12+a161x11+a155x10+a187x9+a145x8+a98x7+a6x6+a122x5+a100x4+a122x3+a100x2+a122x1+a100x0")
    )

   (test-case
    "test-error-code"
    
    (error-code '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17) 1 "M")
    )

   ))

(run-tests test-error-correct-code)