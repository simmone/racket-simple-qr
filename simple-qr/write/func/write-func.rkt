#lang racket

(provide (contract-out
          [cut-string (-> string? list)]
          [to-message-poly (-> list? string?)]
          ))

(define (cut-string str)
  (reverse
   (let loop ([loop_str str]
              [result_list '()])
     (cond 
      [(> (string-length loop_str) 8)
       (loop (substring loop_str 8) (cons (substring loop_str 0 8) result_list))]
      [(> (string-length loop_str) 0)
       (cons loop_str result_list)]
      [else
       result_list]))))

(define (to-message-poly number_list)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_list number_list])
        (when (not (null? loop_list))
              (printf "~ax~a" (car loop_list) (sub1 (length loop_list)))
              (when (> (length loop_list) 1)
                    (printf "+"))
              (loop (cdr loop_list)))))))

