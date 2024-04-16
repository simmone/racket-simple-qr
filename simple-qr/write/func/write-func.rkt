#lang racket

(provide (contract-out
          [interleave-list (-> list? list?)]
          [decimal-list-to-string (-> list? string?)]
          [cut-string (-> string? list)]
          [to-message-poly (-> list? string?)]
          ))

(define (interleave-list data_list)
  (let loop ([count 0]
             [result_list '()])
    (let ([temp_list '()])
      (for-each
       (lambda (item_list)
         (when (<= count (sub1 (length item_list)))
               (set! temp_list `(,@temp_list ,(list-ref item_list count)))))
       data_list)
      
      (if (null? temp_list)
          result_list
          (loop (add1 count) `(,@result_list ,@temp_list))))))

(define (decimal-list-to-string decimal_list)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (num)
         (printf "~a" (~r num #:base 2 #:min-width 8 #:pad-string "0")))
       decimal_list))))

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

