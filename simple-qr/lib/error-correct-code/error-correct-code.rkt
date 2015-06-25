#lang racket

(provide (contract-out 
          [error-code (-> list? exact-nonnegative-integer? string? list?)]
          [to-message-poly (-> list? string?)]
          ))

(require "../func/code-log/code-log-func.rkt")
(require "../func/code-info/code-info-func.rkt")
(require "../func/poly/poly-dic-func.rkt")
(require "poly-func.rkt")

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
  (let ([ec_count #f]
        [origin_poly_message #f]
        [origin_poly_generator #f]
        [poly_message #f]
        [poly_message_count #f]
        [poly_generator #f]
        )

     (printf "error-code step by step\n")

     (printf "source=[~a]\n" number_list)

    (set! ec_count (get-ec-count version error_level))
     (printf "st1: version=[~a] error_level=[~a] ec_count=[~a]\n" version error_level ec_count)

    (set! origin_poly_message (to-message-poly number_list))
     (printf "st2: origin_poly_message=[~a]\n" origin_poly_message)

    (set! origin_poly_generator (get-poly ec_count))
     (printf "st3: origin_poly_generator=[~a]\n" origin_poly_generator)

    (set! poly_message (poly-multiply-x origin_poly_message ec_count))
    (set! poly_message_count (length (regexp-split #rx"\\+" poly_message)))
     (printf "st4: poly_message=[~a] poly_message_count=[~a]\n" poly_message poly_message_count)
    
    (set! poly_generator (poly-align-on-x origin_poly_generator poly_message))
     (printf "st5: poly_generator=[~a]\n" poly_generator)
    
    (let loop ([loop_poly_generator poly_generator]
               [loop_poly_message poly_message]
               [count 1])
      (if (<= count poly_message_count)
          (begin
             (printf "step:~a message=[~a] generator=[~a]\n" count loop_poly_message loop_poly_generator)

            (let ([loop_poly_generator_aligned_a #f]
                  [loop_poly_generator_aligned_v #f]
                  [loop_poly_message_v #f]
                  [loop_poly_message_result_v #f])
      
              (set! loop_poly_generator_aligned_a (poly-align-on-a loop_poly_generator loop_poly_message))
               (printf "step:~a loop_poly_generator_aligned_a=~a\n" count loop_poly_generator_aligned_a)

              (set! loop_poly_generator_aligned_v (poly-a-to-v loop_poly_generator_aligned_a))
               (printf "step:~a loop_poly_generator_aligned_v=~a\n" count loop_poly_generator_aligned_v)

              (set! loop_poly_message_v (poly-a-to-v loop_poly_message))
               (printf "step:~a loop_poly_message_v=~a\n" count loop_poly_message_v)

              (set! loop_poly_message_result_v (poly-xor loop_poly_message_v loop_poly_generator_aligned_v))
               (printf "step:~a loop_poly_message_v=~a\n" count loop_poly_message_result_v)

              (loop (poly-multiply-x loop_poly_generator -1) (poly-v-to-a loop_poly_message_result_v) (add1 count))))
          (poly-get-codeword (poly-a-to-v loop_poly_message))))))
