#lang racket

(require rackunit/text-ui)

(require rackunit "error-correct-code.rkt")

(require "../func/func.rkt")

(define test-error-correct-code
  (test-suite
   "test-error-correct-code"

   (test-case
    "test-to-message-poly"

    (check-equal? (to-message-poly '(0 32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17))
                  "0x16+32x15+91x14+11x13+120x12+209x11+114x10+220x9+77x8+67x7+64x6+236x5+17x4+236x3+17x2+236x1+17x0")
    )

   (test-case
    "test-error-code"

     (check-equal?
      (error-code 
       '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17) 1 "M")
      '(196  35  39  119  235  215  231  226  93  23))

     (check-equal?
      (error-code '(67 85 70 134 87 38 85 194 119 50 6 18 6 103 38) 5 "Q")
      '(213 199 11 45 115 247 241 223 229 248 154 117 154 111 86 161 111 39))

     (check-equal?
      (error-code '(70 247 118 86 194 6 151 50 16 236 17 236 17 236 17 236) 5 "Q")
      '(235 159 5 173 24 147 59 33 106 40 255 172 82 2 131 32 178 236))

     (check-equal?
      (error-code '(64 6 86 135 71 71 3 162 242 247 54 70 182 102 22) 10 "H")
      '(29 23 164 196 164 199 221 231 160 56 120 121 246 189 156 25 193 90 199 51 163 10 30 57 126 35 216 133))

     (check-equal?
      (error-code '(0 236 17 236 17 236 17 236 17) 4 "H")
      '(207  10  202  186  241  158  215  133  107  98  215  223  247  200  241  71))
     )

    ))

  (run-tests test-error-correct-code)
