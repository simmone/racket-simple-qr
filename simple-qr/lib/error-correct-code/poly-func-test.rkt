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

    (check-equal? (poly-v-to-a "32x25+2x24+101x23+10x22+97x21+0x20+15x19+47x18+134x17+74x16+5x15")
                  "a5x25+a1x24+a72x23+a51x22+a66x21+0x20+a75x19+a69x18+a99x17+a37x16+a50x15")

    (check-equal? (poly-a-to-v "a5x25+a1x24+a72x23+a51x22+a66x21+0x20+a75x19+a69x18+a99x17+a37x16+a50x15")
                               "32x25+2x24+101x23+10x22+97x21+0x20+15x19+47x18+134x17+74x16+5x15")
    )

   (test-case
    "test-poly-xor"

     (let-values ([(skip_count result_str)
                   (poly-xor
                   "27x19+81x18+144x17+170x16+127x15+219x14+223x13+206x12+99x11+101x10+150x9+77x8+21x7+233x6+123x5+71x4+163x3+225x2+154x1"
                   "27x19+248x18+167x17+83x16+224x15+140x14+243x13+206x12+213x11+191x10+138x9+119x8+18x7+132x6+89x5+250x4+113x3+179x2")])
       (check-equal? skip_count 1)
       (check-equal? result_str "169x18+55x17+249x16+159x15+87x14+44x13+0x12+182x11+218x10+28x9+58x8+7x7+109x6+34x5+189x4+210x3+82x2+154x1"))

     (let-values ([(skip_count result_str)
                   (poly-xor
                    "27x19+81x18+144x17+170x16+127x15+219x14+223x13+206x12+99x11+101x10+150x9+77x8+21x7+233x6+123x5+71x4+163x3+225x2+154x1"
                    "27x19+81x18+167x17+83x16+224x15+140x14+243x13+206x12+213x11+191x10+138x9+119x8+18x7+132x6+89x5+250x4+113x3+179x2")])
       (check-equal? skip_count 2)
       (check-equal? result_str "55x17+249x16+159x15+87x14+44x13+0x12+182x11+218x10+28x9+58x8+7x7+109x6+34x5+189x4+210x3+82x2+154x1"))

     (let-values ([(skip_count result_str)
                   (poly-xor
                    "27x19+81x18+167x17+83x16+224x15+219x14+223x13+206x12+99x11+101x10+150x9+77x8+21x7+233x6+123x5+71x4+163x3+225x2+154x1"
                    "27x19+81x18+167x17+83x16+224x15+140x14+243x13+206x12+213x11+191x10+138x9+119x8+18x7+132x6+89x5+250x4+113x3+179x2")])
       (check-equal? skip_count 5)
       (check-equal? result_str "87x14+44x13+0x12+182x11+218x10+28x9+58x8+7x7+109x6+34x5+189x4+210x3+82x2+154x1"))

     (let-values ([(skip_count result_str)
                   (poly-xor
                    "32x25+91x24+11x23+120x22+209x21+114x20+220x19+77x18+67x17+64x16+236x15+17x14+236x13+17x12+236x11+17x10"
                    "32x25+2x24+101x23+10x22+97x21+197x20+15x19+47x18+134x17+74x16+5x15")])
       (check-equal? skip_count 1)
       (check-equal? result_str "89x24+110x23+114x22+176x21+183x20+211x19+98x18+197x17+10x16+233x15+17x14+236x13+17x12+236x11+17x10"))

     (let-values ([(skip_count result_str)
                   (poly-xor
                    "135x19+141x18+255x17+182x16+62x15+100x14+49x13+97x12+230x11+107x10+0x9"
                    "135x19+58x18+253x17+210x16+137x15+92x14+187x13+60x12+104x11+193x10+105x9")])
       (check-equal? skip_count 1)
       (check-equal? result_str "183x18+2x17+100x16+183x15+56x14+138x13+93x12+142x11+170x10+105x9"))
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
