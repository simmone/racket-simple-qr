#lang racket

(require rackunit/text-ui)

(require rackunit "data-encoding.rkt")

(require "../func/func.rkt")

(define test-data-encoding
  (test-suite
   "test-data-encoding"

   (test-case
    "test-locate-data-encoding-joints"

    (let* ([url "http://chenxiao.info"]
           [mode "B"]
           [error_level "H"]
           [character_count (string-length url)]
           [version (get-version url mode error_level)]
           [mode_indicator (get-mode-indicator mode)]
           [character_count_indicator (get-character-count-indicator character_count version mode)]
           )
      (check-equal? mode_indicator "0100")
      (check-equal? character_count_indicator "00010100")))

   (test-case
    "test-encode-b"

    (check-equal? (encode-b "Hello") "0100100001100101011011000110110001101111")
    )

   (test-case
    "test-num-split-three"

    (check-equal? (num-split-three "10000") '(100 0))
    (check-equal? (num-split-three "10001") '(100 1))
    (check-equal? (num-split-three "12345") '(123 45))
    (check-equal? (num-split-three "1234567") '(123 456 7))
    (check-equal? (num-split-three "123456") '(123 456))
    (check-equal? (num-split-three "1") '(1))
    (check-equal? (num-split-three "12") '(12))
    (check-equal? (num-split-three "123") '(123))
    (check-equal? (num-split-three "1234") '(123 4))
    (check-equal? (num-split-three "01234") '(12 34))
    (check-equal? (num-split-three "001234") '(1 234))
    (check-equal? (num-split-three "0001234") '(0 123 4))
    )

   (test-case
    "test-encode-n"
    
    (check-equal? (encode-n "1234567") "1111011111001000111")
    )
    
   (test-case
    "test-string-split-two"

    (check-equal? (string-split-two "A") '("A"))
    (check-equal? (string-split-two "AB") '("AB"))
    (check-equal? (string-split-two "ABC") '("AB" "C"))
    (check-equal? (string-split-two "ABCD") '("AB" "CD"))
    )

   (test-case
    "test-encode-a"
    
    (check-equal? (encode-a "HEA") "01100001011001010")
    )

   (test-case
    "test-get-required-bits-width"
    
    (check-equal? (get-required-bits-width 2 "M") 224)
    )

   (test-case
    "test-data-encode"
    
    (check-equal? (data-encode "HELLO WORLD" #:version 1 #:mode "A" #:error_level "Q")
                  "00100000010110110000101101111000110100010111001011011100010011010100001101000000111011000001000111101100")

    (check-equal? (data-encode "HELLO WORLD" #:version 1 #:mode "A" #:error_level "M")
                  "00100000010110110000101101111000110100010111001011011100010011010100001101000000111011000001000111101100000100011110110000010001")

    (parameterize 
     ([*trace_level* 2])
     (check-equal? (data-encode "01234567" #:version 1 #:mode "N" #:error_level "M")
                   "00010000001000000000110001010110011000011000000011101100000100011110110000010001111011000001000111101100000100011110110000010001"))
    )

   ))

(run-tests test-data-encoding)
