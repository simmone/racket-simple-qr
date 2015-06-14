#lang racket

(provide (contract-out 
          [get-version (-> string? string? string? exact-nonnegative-integer?)]
          [get-mode-indicator (-> string? string?)]
          [get-character-count-indicator (-> exact-nonnegative-integer? exact-nonnegative-integer? string? string?)]
          [encode-b (-> string? string?)]
          [num-split-three (-> string? list?)]
          [encode-n (-> string? string?)]
          ))

(require "../func/global.rkt")
(require "../func/capacity/capacity-func.rkt")
(require "../func/character-count/character-bit-width.rkt")

(require racket/format)

(define (get-version content mode error_level)
  (get-version-origin (string-length content) mode error_level))

(define (get-mode-indicator mode)
  (hash-ref *mode_bit_table* mode))

(define (get-character-count-indicator character_count version mode)
  (~r character_count #:base 2 #:min-width (get-character-bit-width version mode) #:pad-string "0"))

(define (encode-b content)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (char_byte)
         (printf "~a" (~r char_byte #:base 2 #:min-width 8 #:pad-string "0")))
       (bytes->list (string->bytes/latin-1 content))))))

(define (num-split-three num_str)
  (reverse
   (let loop ([loop_str num_str]
              [result_list '()])
     (if (<= (string-length loop_str) 3)
         (cons (string->number loop_str) result_list)
         (loop (substring loop_str 3) (cons (string->number (substring loop_str 0 3)) result_list))))))

(define (encode-n content)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (num)
         (printf "~a" (~r num #:base 2)))
       (num-split-three content)))))
