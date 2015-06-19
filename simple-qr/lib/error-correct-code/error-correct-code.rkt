#lang racket

(provide (contract-out 
          [error-code (-> list? exact-nonnegative-integer? string? string?)]
          [to-message-poly (-> list? string?)]
          [poly-multiply-x (-> string? exact-nonnegative-integer? string?)]
          [poly-align-on-x (-> string? string? string?)]
          ))

(require "../func/code-log/code-log-func.rkt")
(require "../func/code-info/code-info-func.rkt")
(require "../func/poly/poly-func.rkt")

(define (to-message-poly number_list)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_list number_list])
        (when (not (null? loop_list))
              (printf "a~ax~a" (value->a (car loop_list)) (sub1 (length loop_list)))
              (when (> (length loop_list) 1)
                    (printf "+"))
              (loop (cdr loop_list)))))))

;; (a1x2+a3x4)*x2 = a1x4+a3x6
(define (poly-multiply-x poly_str x_log)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_list (regexp-split #rx"\\+" poly_str)])
        (when (not (null? loop_list))
              (let* ([result (regexp-match #rx"a([0-9]+)x([0-9]+)" (car loop_list))]
                     [a_index (list-ref result 1)]
                     [x_index (list-ref result 2)])
                (printf "a~ax~a" a_index (+ (string->number x_index) x_log)))
              (when (> (length loop_list) 1)
                  (printf "+"))
              (loop (cdr loop_list)))))))

;; a0x2+a1x9 align a0x10 = a0x10+a1x17
(define (poly-align-on-x poly ref_poly)
  (let* ([poly_first_item (car (regexp-split #rx"\\+" poly))]
         [ref_poly_first_item (car (regexp-split #rx"\\+" ref_poly))]
         [poly_x (string->number (second (regexp-match #rx"a[0-9]+x([0-9+]+)" poly_first_item)))]
         [ref_poly_x (string->number (second (regexp-match #rx"a[0-9]+x([0-9+]+)" ref_poly_first_item)))])
    (poly-multiply-x poly (- ref_poly_x poly_x))))

(define (error-code number_list version error_level)
  (let ([error_code ""]
        [ec_count #f]
        [origin_message_poly #f]
        [origin_generator_poly #f]
        [message_poly #f]
        [generator_poly #f]
        )

    (printf "error-code step by step\n")

    (printf "source=[~a]\n" number_list)

    (set! ec_count (get-ec-count version error_level))
    (printf "st1: version=[~a] error_level=[~a] ec_count=[~a]\n" version error_level ec_count)

    (set! origin_message_poly (to-message-poly number_list))
    (printf "st2: origin_message_poly=[~a]\n" origin_message_poly)

    (set! origin_generator_poly (get-poly ec_count))
    (printf "st3: origin_generator_poly=[~a]\n" origin_generator_poly)

    (set! message_poly (poly-multiply-x origin_message_poly ec_count))
    (printf "st4: message_poly=[~a]\n" message_poly)
    
    (set! generator_poly (poly-align-on-x origin_generator_poly message_poly))
    (printf "st5: generator_poly=[~a]\n" generator_poly)
    
    error_code))
