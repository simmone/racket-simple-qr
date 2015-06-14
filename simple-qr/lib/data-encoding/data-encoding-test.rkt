#lang racket

(require rackunit/text-ui)

(require rackunit "data-encoding.rkt")

(define test-data-encoding
  (test-suite 
   "test-data-encoding"

   (test-case 
    "test-locate-data-encoding-joints"
    
    (let* ([url "http://chenxiao.info"]
           [mode "B"]
           [error_level "H"]
           [character-count (string-length url)]
           [version (get-version url mode error_level)]
           [mode_indicator (get-mode-indicator mode)]
           [character_count_indicator (get-character-count-indicator character-count)]
           )
      (check-equal? mode_indicator "0100")
      (check-equal? character_count_indicator "000001011")
    )
   
   ))

(run-tests test-data-encoding)
