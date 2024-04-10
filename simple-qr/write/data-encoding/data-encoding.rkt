#lang racket

(provide (contract-out 
          [get-character-count-indicator (-> exact-nonnegative-integer? exact-nonnegative-integer? string? string?)]
          [encode-b (-> string? string?)]
          [encode-n (-> string? string?)]
          [string-split (-> string? exact-nonnegative-integer? list?)]
          [encode-a (-> string? string?)]
          [interleave-data-group (-> list? list?)]
          ))

(require "alphanumeric.rkt"
         "../func/write-func.rkt"
         "../func/code-info/code-info-func.rkt"
         "../func/remainder-bits/remainder-bits-func.rkt"
         "../../share/data-encoding.rkt"
         "../../share/lib.rkt"
         "../../share/data-group.rkt"
         "../../share/character-bit-width.rkt"
         racket/format)

(define (get-character-count-indicator character_count version mode)
  (~r character_count #:base 2 #:min-width (get-character-bit-width version mode) #:pad-string "0"))

(define (encode-b content)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (char_byte)
         (printf "~a" (~r char_byte #:base 2 #:min-width 8 #:pad-string "0")))
       (bytes->list (string->bytes/utf-8 content))))))

(define (string-split str num)
  (reverse
   (let loop ([loop_str str]
              [result_list '()])
     (if (<= (string-length loop_str) num)
         (cons loop_str result_list)
         (loop (substring loop_str num) (cons (substring loop_str 0 num) result_list))))))

(define (encode-n content)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (num)
         (cond
          [(= (string-length num) 3)
           (printf "~a" (~r (string->number num) #:base 2 #:min-width 10 #:pad-string "0"))]
          [(= (string-length num) 2)
           (printf "~a" (~r (string->number num) #:base 2 #:min-width 7 #:pad-string "0"))]
          [(= (string-length num) 1)
           (printf "~a" (~r (string->number num) #:base 2 #:min-width 4 #:pad-string "0"))]))
       (string-split content 3)))))

(define (encode-a str)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (num_pair)
         (if (= (string-length num_pair) 1)
             (printf "~a" (~r (get-alphanumeric-num num_pair) #:base 2 #:min-width 6 #:pad-string "0"))
             (let* ([num_1 (substring num_pair 0 1)]
                    [num_2 (substring num_pair 1 2)]
                    [num_1_value (get-alphanumeric-num num_1)]
                    [num_2_value (get-alphanumeric-num num_2)]
                    [value (+ (* num_1_value 45) num_2_value)])
               (printf "~a" (~r value #:base 2 #:min-width 11 #:pad-string "0")))))
       (string-split str 2)))))

(define (interleave-data-group data_group)
  (let ([data_list
         (quasiquote
          (
           (unquote-splicing (map car (car data_group)))
           (unquote-splicing (map car (cadr data_group)))))]
        [ec_list
         (quasiquote
          (
          (unquote-splicing (map cadr (car data_group)))
          (unquote-splicing (map cadr (cadr data_group)))))])
    `(,@(interleave-list data_list) ,@(interleave-list ec_list))))
