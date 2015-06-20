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
    "test-poly-multiply-x"
    
    (check-equal? (poly-multiply-x "a2x1+a3x0" 5) "a2x6+a3x5")
    (check-equal? (poly-multiply-x "a2x1+a3x0+a4x8" 10) "a2x11+a3x10+a4x18")
    )

   (test-case
    "test-poly-align-on-x"
    
    (check-equal? (poly-align-on-x "a1x1+a2x4" "a0x7") "a1x7+a2x10")
    (check-equal? (poly-align-on-x "a1x1+a2x4" "a0x7+a4x8") "a1x7+a2x10")

    )

   (test-case
    "test-poly-multiply-a"
    
    (check-equal? (poly-multiply-a "a2x1+a3x0" 5) "a7x1+a8x0")
    (check-equal? (poly-multiply-a "a2x1+a3x0+a4x8" 10) "a12x1+a13x0+a14x8")
    )

   (test-case
    "test-poly-align-on-a"
    
    (check-equal? (poly-align-on-a "a1x1+a2x4" "a7x7") "a7x1+a8x4")
    (check-equal? (poly-align-on-a "a1x1+a2x4" "a7x7+a4x8") "a7x1+a8x4")

    (check-equal? (poly-align-on-a "a1x1+a250x4" "a6x7") "a6x1+a0x4")
    (check-equal? (poly-align-on-a "a1x1+a251x4" "a6x7") "a6x1+a1x4")
    )

   (test-case
    "test-poly-value-to-a, a-to-value"
    
    (check-equal? (poly-a-to-value "a5x25+a1x24+a72x23+a51x22+a66x21+a123x20+a75x19+a69x18+a99x17+a37x16+a50x15")
                  "32x25+2x24+101x23+10x22+97x21+197x20+15x19+47x18+134x17+74x16+5x15")

    (check-equal? (poly-value-to-a "32x25+2x24+101x23+10x22+97x21+197x20+15x19+47x18+134x17+74x16+5x15")
                  "a5x25+a1x24+a72x23+a51x22+a66x21+a123x20+a75x19+a69x18+a99x17+a37x16+a50x15")
    )

   (test-case
    "test-error-code"
    
;    (error-code '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17) 1 "M")
    )

   ))

(run-tests test-error-correct-code)
