#lang racket

(provide (contract-out 
          [poly-multiply-x (-> string? exact-nonnegative-integer? string?)]
          [poly-align-on-x (-> string? string? string?)]
          [poly-multiply-a (-> string? exact-nonnegative-integer? string?)]
          [poly-align-on-a (-> string? string? string?)]
          [poly-a-to-value (-> string? string?)]
          [poly-value-to-a (-> string? string?)]
          [poly-xor (-> string? string? string?)]
          ))

(require "../func/code-log/code-log-func.rkt")
(require "../func/code-info/code-info-func.rkt")
(require "../func/poly/poly-dic-func.rkt")

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

;; (a1x2+a3x4)*a2 = a2x2+a5x4
(define (poly-multiply-a poly_str a_log)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_list (regexp-split #rx"\\+" poly_str)])
        (when (not (null? loop_list))
              (let* ([result (regexp-match #rx"a([0-9]+)x([0-9]+)" (car loop_list))]
                     [a_index (list-ref result 1)]
                     [x_index (list-ref result 2)])
                (printf "a~ax~a" (remainder (+ (string->number a_index) a_log) 255) x_index))
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

;; a0x2+a1x9 align a9x10 = a9x2+a10x9
(define (poly-align-on-a poly ref_poly)
  (let* ([poly_first_item (car (regexp-split #rx"\\+" poly))]
         [ref_poly_first_item (car (regexp-split #rx"\\+" ref_poly))]
         [poly_a (string->number (second (regexp-match #rx"a([0-9]+)x[0-9+]+" poly_first_item)))]
         [ref_poly_a (string->number (second (regexp-match #rx"a([0-9]+)x[0-9+]+" ref_poly_first_item)))])
    (poly-multiply-a poly (- ref_poly_a poly_a))))

(define (poly-a-to-value poly_str)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_list (regexp-split #rx"\\+" poly_str)])
        (when (not (null? loop_list))
              (let* ([result (regexp-match #rx"a([0-9]+)x([0-9]+)" (car loop_list))]
                     [a_index (list-ref result 1)]
                     [x_index (list-ref result 2)])
                (printf "~ax~a" (a->value (string->number a_index)) x_index))
              (when (> (length loop_list) 1)
                  (printf "+"))
              (loop (cdr loop_list)))))))

(define (poly-value-to-a poly_str)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_list (regexp-split #rx"\\+" poly_str)])
        (when (not (null? loop_list))
              (let* ([result (regexp-match #rx"([0-9]+)x([0-9]+)" (car loop_list))]
                     [a_index (list-ref result 1)]
                     [x_index (list-ref result 2)])
                (printf "a~ax~a" (value->a (string->number a_index)) x_index))
              (when (> (length loop_list) 1)
                  (printf "+"))
              (loop (cdr loop_list)))))))

(define (poly-xor message_poly generator_poly)
  (with-output-to-string
    (lambda ()
      (let ([message_term_list (regexp-split #rx"\\+" message_poly)]
            [generator_list (regexp-split #rx"\\+" generator_poly)])
        (let loop ([message_loop_list message_term_list]
                   [generator_loop_list generator_list])
          (when (not (null? message_loop_list))
                (let* ([message_result (regexp-match #rx"([0-9]+)x([0-9]+)" (car message_loop_list))]
                       [message_a_index (string->number (list-ref message_result 1))]
                       [message_x_index (string->number (list-ref message_result 2))]
                       [generator_a_index 0])
                  (when (not (null? generator_loop_list))
                        (let ([generator_result (regexp-match #rx"([0-9]+)x([0-9]+)" (car generator_loop_list))])
                          (set! generator_a_index (string->number (list-ref generator_result 1)))))

                  (let ([result_index (bitwise-xor message_a_index generator_a_index)])
                    (when (not (= result_index 0))
                          (printf "~ax~a" result_index message_x_index)

                          (when (> (length message_loop_list) 1)
                                (printf "+")))))
                (loop (cdr message_loop_list) (if (null? generator_loop_list) '() (cdr generator_loop_list)))))))))
