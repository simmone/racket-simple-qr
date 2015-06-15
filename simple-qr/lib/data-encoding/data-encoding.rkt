#lang racket

(provide (contract-out 
          [get-version (-> string? string? string? exact-nonnegative-integer?)]
          [get-mode-indicator (-> string? string?)]
          [get-character-count-indicator (-> exact-nonnegative-integer? exact-nonnegative-integer? string? string?)]
          [encode-b (-> string? string?)]
          [num-split-three (-> string? list?)]
          [encode-n (-> string? string?)]
          [string-split-two (-> string? list?)]
          [encode-a (-> string? string?)]
          [get-required-bits-width (-> exact-nonnegative-integer? string? exact-nonnegative-integer?)]
          [data-encode (-> string? string?)]
          ))

(require "../func/global.rkt")
(require "../func/func.rkt")
(require "../func/capacity/capacity-func.rkt")
(require "../func/capacity/capacity-dic.rkt")
(require "../func/character-count/character-bit-width.rkt")
(require "../func/required-bits/required-bits.rkt")

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

(define (string-split-two str)
  (reverse
   (let loop ([loop_str str]
              [result_list '()])
     (if (<= (string-length loop_str) 2)
         (cons loop_str result_list)
         (loop (substring loop_str 2) (cons (substring loop_str 0 2) result_list))))))

(define (encode-a str)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (num_pair)
         (if (= (string-length num_pair) 1)
             (printf "~a" (~r (hash-ref *alphanumeric_num_table* num_pair) #:base 2 #:min-width 6 #:pad-string "0"))
             (let* ([num_1 (substring num_pair 0 1)]
                    [num_2 (substring num_pair 1 2)]
                    [num_1_value (hash-ref *alphanumeric_num_table* num_1)]
                    [num_2_value (hash-ref *alphanumeric_num_table* num_2)]
                    [value (+ (* num_1_value 45) num_2_value)])
               (printf "~a" (~r value #:base 2 #:min-width 11 #:pad-string "0")))))
       (string-split-two str)))))

(define (get-required-bits-width version error_level)
  (* 8 (hash-ref *required_bits_table* (string-append (number->string version) "-" error_level))))

(define (data-encode content)
  (let ([mode "A"]
        [error_level "Q"]
        [character_count #f]
        [version #f]
        [mode_indicator #f]
        [character_count_indicator #f]
        [capacity_count #f]
        [encoded_data #f]
        [encoded_data_stage1 #f]
        [encoded_data_stage2 #f]
        [encoded_data_stage3 #f]
        [encoded_data_stage4 #f])
    
    (set! character_count (string-length content))
    (set! version (get-version content mode error_level))
    (set! capacity_count (get-required-bits-width version error_level))
    (set! mode_indicator (get-mode-indicator mode))
    (set! character_count_indicator (get-character-count-indicator character_count version mode))
    (set! encoded_data (encode-a content))

    ;; stage1: data origin
    (set! encoded_data_stage1 (string-append mode_indicator character_count_indicator encoded_data))

    ;; stage2: add terminator
    (set! encoded_data_stage2 (add-terminator encoded_data_stage1 capacity_count))

    ;; stage3: add multiple eight
    (set! encoded_data_stage3 (add-multi-eight encoded_data_stage2))

    ;; stage4: repeat padding
    (set! encoded_data_stage4 (repeat-right-pad-string encoded_data_stage3 capacity_count "1110110000010001"))

    encoded_data_stage4))
