#lang racket

(require rackunit/text-ui)

(require rackunit "../../../write/lib/func/poly/poly-dic-func.rkt")

(define test-poly-dic-func
  (test-suite 
   "test-poly-dic-func"

   (test-case
    "test-get-ploy"

    (check-equal? (get-poly 42)
                  "a0x42+a250x41+a103x40+a221x39+a230x38+a25x37+a18x36+a137x35+a231x34+a0x33+a3x32+a58x31+a242x30+a221x29+a191x28+a110x27+a84x26+a230x25+a8x24+a188x23+a106x22+a96x21+a147x20+a15x19+a131x18+a139x17+a34x16+a101x15+a223x14+a39x13+a101x12+a213x11+a199x10+a237x9+a254x8+a201x7+a123x6+a171x5+a162x4+a194x3+a117x2+a50x1+a96x0")
    )

   ))

(run-tests test-poly-dic-func)
