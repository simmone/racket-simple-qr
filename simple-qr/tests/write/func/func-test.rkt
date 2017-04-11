#lang racket

(require rackunit/text-ui)

(require rackunit "../../../write/lib/func/func.rkt")

(define test-func
  (test-suite 
   "test-func"

   (test-case
    "test-version->modules"

    (check-equal? (version->modules 1) 21)
    (check-equal? (version->modules 2) 25)
    (check-equal? (version->modules 3) 29)
    (check-equal? (version->modules 5) 37)
    (check-equal? (version->modules 39) 173)    
    (check-equal? (version->modules 40) 177)
    )

   (test-case 
    "test-locate-brick"
    
    (let ([place_pair (locate-brick 1 (cons 1 1))])
      (check-equal? place_pair '(0 . 0)))

    (let ([place_pair (locate-brick 2 (cons 1 1))])
      (check-equal? place_pair '(0 . 0)))

    (let ([place_pair (locate-brick 2 (cons 2 2))])
      (check-equal? place_pair '(2 . 2)))

    (let ([place_pair (locate-brick 2 (cons 5 5))])
      (check-equal? place_pair '(8 . 8)))

    (let ([place_pair (locate-brick 3 (cons 5 5))])
      (check-equal? place_pair '(12 . 12)))

    (let ([place_pair (locate-brick 3 (cons 5 7))])
      (check-equal? place_pair '(18 . 12)))
    )

   (test-case
    "test-locate-finder-pattern"

    (let ([start_points (locate-finder-pattern 21)])
      (check-equal? (first start_points) '(1 . 1))
      (check-equal? (second start_points) '(15 . 1))
      (check-equal? (third start_points) '(1 . 15)))
    )

   (test-case
    "test-transform-points"

    (let* ([points_list
            '((1 . 2) (3 . 4) (4 . 3))]
           [transformed_points_list #f])
      
      (set! transformed_points_list
            (transform-points-list points_list '(1 . 1)))
      (check-equal? transformed_points_list '((1 . 2) (3 . 4) (4 . 3)))

      (set! transformed_points_list
            (transform-points-list points_list '(1 . 2)))
      (check-equal? transformed_points_list '((1 . 3) (3 . 5) (4 . 4)))

      ))

   (test-case
    "test-add-terminator"
    
    (check-equal? (add-terminator "1234" 4) "1234")
    (check-equal? (add-terminator "1234" 5) "12340")
    (check-equal? (add-terminator "1234" 6) "123400")
    (check-equal? (add-terminator "1234" 7) "1234000")
    (check-equal? (add-terminator "1234" 8) "12340000")
    (check-equal? (add-terminator "1234" 9) "12340000")
    (check-equal? (add-terminator "1234" 10) "12340000")
    (check-equal? (add-terminator "1234" 10) "12340000")
    )

   (test-case
    "test-add-multi-eight"
    
    (check-equal? (add-multi-eight "1234") "12340000")
    (check-equal? (add-multi-eight "123456") "12345600")
    (check-equal? (add-multi-eight "1234567") "12345670")
    (check-equal? (add-multi-eight "12345678") "12345678")
    (check-equal? (add-multi-eight "123456789") "1234567890000000")
    (check-equal? (add-multi-eight "12345678123456789") "123456781234567890000000")
    )

   (test-case
    "test-repeat-right-pad-string"
    
    (check-equal? (repeat-right-pad-string "123" 10 "456") "1234564564")
    (check-equal? (repeat-right-pad-string "123" 11 "456") "12345645645")
    (check-equal? (repeat-right-pad-string "123" 12 "456") "123456456456")
    (check-equal? (repeat-right-pad-string "123" 10 "1234567890") "1231234567")
    )

   (test-case
    "test-split-bit-string-to-decimal"
    
    (check-equal?
     (split-bit-string-to-decimal 
      "00100000010110110000101101111000110100010111001011011100010011010100001101000000111011000001000111101100000100011110110000010001")
     '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17))
    
    (check-equal?
     (split-bit-string-to-decimal
      (string-append
       "010000110101010101000110100001100101011100100110010101011100001001110111001100100000011000010010000001100110011100100110"
       "111101101111011001000010000001110111011010000110111100100000011100100110010101100001011011000110110001111001001000000110"
       "10110110111001101111011101110111001100100000011101110110100001100101011100100110010100100000011010000110100101110011001000000111"
       "01000110111101110111011001010110110000100000011010010111001100101110000011101100000100011110110000010001111011000001000111101100"))
      '(67 85 70 134 87 38 85 194 119 50 6 18 6 103 38 246 246 66 7 118 134 242 7 38 86 22 198 199 146 6 182 230 247 119 50 7 118 134 87 38 82 6 134 151 50 7 70 247 118 86 194 6 151 50 224 236 17 236 17 236 17 236))
    
    (check-equal?
     (string->number "#b11100000")
     224)
    )

   (test-case
    "test-split-decimal-list-on-contract"
    
    (let ([result_list
           (split-decimal-list-on-contract 
            '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17)
            #((1 . 16) (0 . 0)))])
      (check-equal? result_list '(((32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17)) ())))

    (let ([result_list
           (split-decimal-list-on-contract 
            '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17)
            #((2 . 8) (0 . 0)))])
      (check-equal? result_list '(((32 91 11 120 209 114 220 77) (67 64 236 17 236 17 236 17)) ())))

    (let ([result_list
           (split-decimal-list-on-contract 
            '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17)
            #((1 . 8) (1 . 8)))])
      (check-equal? result_list '(((32 91 11 120 209 114 220 77)) ((67 64 236 17 236 17 236 17)))))

    (let ([result_list
           (split-decimal-list-on-contract 
            '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17)
            #((1 . 6) (2 . 5)))])
      (check-equal? result_list '(((32 91 11 120 209 114)) ((220 77 67 64 236) (17 236 17 236 17)))))

    (let ([result_list
           (split-decimal-list-on-contract 
            '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17)
            #((2 . 5) (1 . 6)))])
      (check-equal? result_list '(((32 91 11 120 209) (114 220 77 67 64)) ((236 17 236 17 236 17)))))

    )

   (test-case
    "test-interleave-list"
    
    (check-equal? (interleave-list '((1 2 3 4) (1))) '(1 1 2 3 4))

    (check-equal? (interleave-list '((1 2 3 4) (5 6 7))) '(1 5 2 6 3 7 4))

    (check-equal? (interleave-list '((1 2 3 4 5) (5 6 7))) '(1 5 2 6 3 7 4 5))

    (check-equal? (interleave-list '((1 2 3 4 5) (5 6 7) (3))) '(1 5 3 2 6 3 7 4 5))

    (check-equal? (interleave-list '((1 2 3 4 5) (5 6 7) (8 9 10 11 12 13 14))) '(1 5 8 2 6 9 3 7 10 4 11 5 12 13 14))
    )

   (test-case
    "test-decimal-list-to-string"

    (check-equal? 
     (decimal-list-to-string
      '(67 246 182 70 85 246 230 247 70 66 247 118 134 7 119 86 87 118 50 194 38 134 7 6 85 242 118 151 194 7 134 50 119 38 87 16 50 86 38 236 6 22 82 17 18 198 6 236 6 199 134 17 103 146 151 236 38 6 50 17 7 236 213 87 148 235 199 204 116 159 11 96 177 5 45 60 212 173 115 202 76 24 247 182 133 147 241 124 75 59 223 157 242 33 229 200 238 106 248 134 76 40 154 27 195 255 117 129 230 172 154 209 189 82 111 17 10 2 86 163 108 131 161 163 240 32 111 120 192 178 39 133 141 236))
     "0100001111110110101101100100011001010101111101101110011011110111010001100100001011110111011101101000011000000111011101110101011001010111011101100011001011000010001001101000011000000111000001100101010111110010011101101001011111000010000001111000011000110010011101110010011001010111000100000011001001010110001001101110110000000110000101100101001000010001000100101100011000000110111011000000011011000111100001100001000101100111100100101001011111101100001001100000011000110010000100010000011111101100110101010101011110010100111010111100011111001100011101001001111100001011011000001011000100000101001011010011110011010100101011010111001111001010010011000001100011110111101101101000010110010011111100010111110001001011001110111101111110011101111100100010000111100101110010001110111001101010111110001000011001001100001010001001101000011011110000111111111101110101100000011110011010101100100110101101000110111101010100100110111100010001000010100000001001010110101000110110110010000011101000011010001111110000001000000110111101111000110000001011001000100111100001011000110111101100")
    )

   (test-case
    "test-cut-string"
    
    (check-equal? (cut-string "123456781234") '("12345678" "1234"))
    (check-equal? (cut-string "12345678123456789") '("12345678" "12345678" "9"))
    )

   ))

(run-tests test-func)
