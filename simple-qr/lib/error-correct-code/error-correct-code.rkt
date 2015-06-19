#lang racket

(provide (contract-out 
          [error-code (-> list? exact-nonnegative-integer? string? string?)]
          [to-message-poly (-> list? string?)]
          ))

(require "../func/code-log/code-log-func.rkt")
(require "../func/code-info/code-info-func.rkt")

(define (to-message-poly number_list)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_list number_list])
        (when (not (null? loop_list))
              (printf "a~ax~a" (value->a (car loop_list)) (sub1 (length loop_list)))
              (when (> (length loop_list) 1)
                    (printf "+"))
              (loop (cdr loop_list)))))))


(define (error-code number_list version error_level)
  (let ([error_code ""]
        [ec_count #f]
        [message_poly #f])

    (printf "error-code step by step\n")

    (printf "source=[~a]\n" number_list)

    (set! ec_count (get-ec-count version error_level))
    (printf "st1:version=[~a] error_level=[~a] ec_count=[~a]\n" version error_level ec_count)

    (set! message_poly (to-message-poly number_list))
    (printf "st2:message_poly=[~a]\n" message_poly)
    
    error_code))
