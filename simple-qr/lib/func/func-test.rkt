#lang racket

(require rackunit/text-ui)

(require rackunit "func.rkt")

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
    "test-get-points-between"

    (check-equal?
     (get-points-between '(1 . 1) '(1 . 10) #:direction 'horizontal)
     '((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6) (1 . 7) (1 . 8) (1 . 9) (1 . 10)))

    (check-equal?
     (get-points-between '(1 . 1) '(1 . 10) #:direction 'vertical)
     '())

    (check-equal?
     (get-points-between '(1 . 1) '(5 . 1) #:direction 'vertical)
     '((1 . 1) (2 . 1) (3 . 1) (4 . 1) (5 . 1)))

    (check-equal?
     (get-points-between '(1 . 1) '(5 . 1) #:direction 'horizontal)
     '())

    (check-equal?
     (get-points-between '(1 . 1) '(5 . 2) #:direction 'vertical)
     '())
    
    )

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

   ))

(run-tests test-func)
