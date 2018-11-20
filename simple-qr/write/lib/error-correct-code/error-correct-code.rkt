#lang racket

(provide (contract-out 
          [error-code (-> natural? string? list? list?)]
          ))

(require "../func/code-log/code-log-func.rkt")
(require "../func/code-info/code-info-func.rkt")
(require "../func/poly/poly-dic-func.rkt")
(require "../func/func.rkt")
(require "../../../share/func.rkt")
(require "poly-func.rkt")
(require "../express/express.rkt")

(define (error-code ec_count origin_poly_generator number_list)
  (let ([origin_poly_message #f]
        [poly_message #f]
        [poly_message_count #f]
        [poly_generator #f]
        )

    (set! origin_poly_message (to-message-poly number_list))
    (set! poly_message (message-multiply-x origin_poly_message ec_count))
    (set! poly_message_count (length (regexp-split #rx"\\+" poly_message)))
    (set! poly_generator (poly-align-on-x origin_poly_generator poly_message))

    (let loop ([loop_poly_generator poly_generator]
               [loop_poly_message poly_message]
               [count 1])
      (if (<= count poly_message_count)
          (begin
            (let ([loop_poly_generator_aligned_a #f]
                  [loop_poly_generator_aligned_v #f])
                  
              (if (regexp-match #rx"^0x.*" loop_poly_message)
                  (set! loop_poly_generator_aligned_v (poly-to-zero loop_poly_generator))
                  (begin
                    (set! loop_poly_generator_aligned_a (poly-align-on-a loop_poly_generator loop_poly_message))
              
                    (set! loop_poly_generator_aligned_v (poly-a-to-v loop_poly_generator_aligned_a))))

              (let-values ([(skip_count loop_poly_message_result_v)
                            (poly-xor loop_poly_message loop_poly_generator_aligned_v)])
                (loop (poly-multiply-x loop_poly_generator (* skip_count -1)) loop_poly_message_result_v (+ count skip_count)))))
            (poly-get-codeword loop_poly_message)))))
