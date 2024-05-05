#lang racket

(require rackunit/text-ui
         rackunit
         "../../share/qr.rkt"
         "../../write/data-encoding.rkt")

(define test-data-encoding
  (test-suite
   "test-data-encoding"

   (test-case
    "test-locate-data-encoding-joints"

    (let* ([url "http://chenxiao.info"]
           [mode 'B]
           [error_level 'H]
           [character_count (string-length url)]
           [version (get-version character_count mode error_level)]
           [count_bit_width (get-character-bit-width version mode)]
           [character_count_indicator (~r character_count #:base 2 #:min-width count_bit_width #:pad-string "0")]
           )
      (check-equal? character_count_indicator "00010100")))

   (test-case
    "test-encode-b"

    (check-equal? (encode-b "Hello") "0100100001100101011011000110110001101111")
    )

   (test-case
    "test-encode-n"
    
    (check-equal? (encode-n "1234567") "000111101101110010000111")
    )
   
   (test-case
    "test-string-split"

    (check-equal? (string-split "A" 2) '("A"))
    (check-equal? (string-split "AB" 2) '("AB"))
    (check-equal? (string-split "ABC" 2) '("AB" "C"))
    (check-equal? (string-split "ABCD" 2) '("AB" "CD"))

    (check-equal? (string-split "10000" 3) '("100" "00"))
    (check-equal? (string-split "10001" 3) '("100" "01"))
    (check-equal? (string-split "12345" 3) '("123" "45"))
    (check-equal? (string-split "1234567" 3) '("123" "456" "7"))
    (check-equal? (string-split "123456" 3) '("123" "456"))
    (check-equal? (string-split "1" 3) '("1"))
    (check-equal? (string-split "12" 3) '("12"))
    (check-equal? (string-split "123" 3) '("123"))
    (check-equal? (string-split "1234" 3) '("123" "4"))
    (check-equal? (string-split "01234" 3) '("012" "34"))
    (check-equal? (string-split "001234" 3) '("001" "234"))
    (check-equal? (string-split "0001234" 3) '("000" "123" "4"))
    )

   (test-case
    "test-encode-a"
    
    (check-equal? (encode-a "HEA") "01100001011001010")
    )

   (test-case
    "test-get-character-bit-width"

    (check-equal? (get-character-bit-width 1 'N) 10)
    (check-equal? (get-character-bit-width 1 'A) 9)
    (check-equal? (get-character-bit-width 1 'B) 8)

    (check-equal? (get-character-bit-width 10 'N) 12)
    (check-equal? (get-character-bit-width 26 'A) 11)

    (check-equal? (get-character-bit-width 27 'N) 14)
    (check-equal? (get-character-bit-width 40 'A) 13)
    )

   ))

(run-tests test-data-encoding)
