#lang racket

(require rackunit/text-ui)

(require rackunit "poly-func.rkt")

(define test-poly-func
  (test-suite
   "test-poly-func"

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
    "test-poly-v-to-a, a-to-v"
    
    (check-equal? (poly-a-to-v "a5x25+a1x24+a72x23+a51x22+a66x21+a123x20+a75x19+a69x18+a99x17+a37x16+a50x15")
                  "32x25+2x24+101x23+10x22+97x21+197x20+15x19+47x18+134x17+74x16+5x15")

    (check-equal? (poly-v-to-a "32x25+2x24+101x23+10x22+97x21+197x20+15x19+47x18+134x17+74x16+5x15")
                  "a5x25+a1x24+a72x23+a51x22+a66x21+a123x20+a75x19+a69x18+a99x17+a37x16+a50x15")
    )

   (test-case
    "test-poly-xor"
    
    (check-equal? (poly-xor 
                   "32x25+91x24+11x23+120x22+209x21+114x20+220x19+77x18+67x17+64x16+236x15+17x14+236x13+17x12+236x11+17x10"
                   "32x25+2x24+101x23+10x22+97x21+197x20+15x19+47x18+134x17+74x16+5x15")
                  "89x24+110x23+114x22+176x21+183x20+211x19+98x18+197x17+10x16+233x15+17x14+236x13+17x12+236x11+17x10")
    (check-equal? (poly-xor
                   "135x19+141x18+255x17+182x16+62x15+100x14+49x13+97x12+230x11+107x10+0x9"
                   "135x19+58x18+253x17+210x16+137x15+92x14+187x13+60x12+104x11+193x10+105x9")
                  "183x18+2x17+100x16+183x15+56x14+138x13+93x12+142x11+170x10+105x9")

    (check-equal? (poly-xor
                   "67x32+85x31+70x30+134x29+87x28+38x27+85x26+194x25+119x24+50x23+6x22+18x21+6x20+103x19+38x18"
                   "67x32+105x31+60x30+2x29+130x28+12x27+100x26+195x25+135x24+219x23+96x22+108x21+207x20+226x19+234x18+136x17+200x16+50x15+216x14")
                  "60x31+122x30+132x29+213x28+42x27+49x26+1x25+240x24+233x23+102x22+126x21+201x20+133x19+204x18+136x17+200x16+50x15+216x14")
    )

   (test-case
    "test-poly-cdr"
    
    (check-equal? (poly-cdr "a0x3+a3x5+a0x3") "a3x5+a0x3")
    )

   (test-case
    "test-poly-append-zero"
    
    (check-equal? (poly-append-zero "1x10+9x9") "1x10+9x9+0x8")
    (check-equal? (poly-append-zero "1x10+9x9+10x8") "1x10+9x9+10x8+0x7")
    )
   
   (test-case
    "test-poly-get-codeword"
    
    (check-equal? (poly-get-codeword "196x9+35x8+39x7+119x6+235x5+215x4+231x3+226x2+93x1+23x0")
                  '(196  35  39  119  235  215  231  226  93  23))
    )

   ))

(run-tests test-poly-func)
