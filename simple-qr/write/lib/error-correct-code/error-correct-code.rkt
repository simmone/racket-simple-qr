#lang racket

(provide (contract-out 
          [error-code (-> list? exact-nonnegative-integer? string? list?)]
          [to-message-poly (-> list? string?)]
          ))

(require "../func/code-log/code-log-func.rkt")
(require "../func/code-info/code-info-func.rkt")
(require "../func/poly/poly-dic-func.rkt")
(require "../func/func.rkt")
(require "../../../share/func.rkt")
(require "poly-func.rkt")

(define (to-message-poly number_list)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_list number_list])
        (when (not (null? loop_list))
              (printf "~ax~a" (car loop_list) (sub1 (length loop_list)))
              (when (> (length loop_list) 1)
                    (printf "+"))
              (loop (cdr loop_list)))))))

(define (error-code number_list version error_level)
  (let ([ec_count #f]
        [origin_poly_message #f]
        [origin_poly_generator #f]
        [poly_message #f]
        [poly_message_count #f]
        [poly_generator #f]
        )

    (appTrace *TRACE_DEBUG* (lambda () (printf "error-code step by step\n")))

    (appTrace *TRACE_DEBUG* (lambda () (printf "source=[~a]\n" number_list)))

    (set! ec_count (get-ec-count version error_level))
    (appTrace *TRACE_DEBUG* (lambda () (printf "st1: version=[~a] error_level=[~a] ec_count=[~a]\n" version error_level ec_count)))

    (set! origin_poly_message (to-message-poly number_list))
    (appTrace *TRACE_DEBUG* (lambda () (printf "st2: origin_poly_message=[~a]\n" origin_poly_message)))

    (set! origin_poly_generator (get-poly ec_count))
    (appTrace *TRACE_DEBUG* (lambda () (printf "st3: origin_poly_generator=[~a]\n" origin_poly_generator)))

    (set! poly_message (message-multiply-x origin_poly_message ec_count))
    (set! poly_message_count (length (regexp-split #rx"\\+" poly_message)))
    (appTrace *TRACE_DEBUG* (lambda () (printf "st4: poly_message=[~a] poly_message_count=[~a]\n" poly_message poly_message_count)))
    
    (set! poly_generator (poly-align-on-x origin_poly_generator poly_message))
    (appTrace *TRACE_DEBUG* (lambda () (printf "st5: poly_generator=[~a]\n" poly_generator)))
    
    (let loop ([loop_poly_generator poly_generator]
               [loop_poly_message poly_message]
               [count 1])
      (if (<= count poly_message_count)
          (begin
            (appTrace *TRACE_DEBUG* (lambda () (printf "step:~a-1 message=[~a] generator=[~a]\n" count loop_poly_message loop_poly_generator)))
            
            (let ([loop_poly_generator_aligned_a #f]
                  [loop_poly_generator_aligned_v #f])
                  
              (if (regexp-match #rx"^0x.*" loop_poly_message)
                  (set! loop_poly_generator_aligned_v (poly-to-zero loop_poly_generator))
                  (begin
                    (set! loop_poly_generator_aligned_a (poly-align-on-a loop_poly_generator loop_poly_message))
                    (appTrace *TRACE_DEBUG* (lambda () (printf "step:~a-2 loop_poly_generator_aligned_a=~a\n" count loop_poly_generator_aligned_a)))
              
                    (set! loop_poly_generator_aligned_v (poly-a-to-v loop_poly_generator_aligned_a))
                    (appTrace *TRACE_DEBUG* (lambda () (printf "step:~a-3 loop_poly_generator_aligned_v=~a\n" count loop_poly_generator_aligned_v)))))

              (let-values ([(skip_count loop_poly_message_result_v)
                            (poly-xor loop_poly_message loop_poly_generator_aligned_v)])
                (appTrace *TRACE_DEBUG* (lambda () (printf "step:~a-4 skip_count=[~a]loop_poly_message_xor_result_v=~a\n" count skip_count loop_poly_message_result_v)))

                (loop (poly-multiply-x loop_poly_generator (* skip_count -1)) loop_poly_message_result_v (+ count skip_count)))))
          (poly-get-codeword loop_poly_message)))))
