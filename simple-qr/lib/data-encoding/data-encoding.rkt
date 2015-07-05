#lang racket

(provide (contract-out 
          [get-version (-> string? string? string? exact-nonnegative-integer?)]
          [get-mode-indicator (-> string? string?)]
          [get-character-count-indicator (-> exact-nonnegative-integer? exact-nonnegative-integer? string? string?)]
          [encode-b (-> string? string?)]
          [encode-n (-> string? string?)]
          [string-split (-> string? exact-nonnegative-integer? list?)]
          [encode-a (-> string? string?)]
          [get-required-bits-width (-> exact-nonnegative-integer? string? exact-nonnegative-integer?)]
          [data-encode (-> string? #:version exact-nonnegative-integer? #:mode string? #:error_level string? string?)]
          ))

(require "alphanumeric.rkt")
(require "../func/func.rkt")
(require "../func/capacity/capacity-func.rkt")
(require "../func/capacity/capacity-dic.rkt")
(require "../func/character-count/character-bit-width.rkt")
(require "../func/code-info/code-info-func.rkt")

(require racket/format)

(define *mode_bit_table* '#hash(("N" . "0001") ("A" . "0010") ("B" . "0100") ("K" . "1000") ("E" . "0111")))

(define (get-mode-indicator mode)
  (hash-ref *mode_bit_table* mode))

(define (get-version content mode error_level)
  (get-version-origin (string-length content) mode error_level))

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
  (trace (format "split:~a\n" (string-split content 3)) 2)
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

(define (get-required-bits-width version error_level)
  (* 8 (get-bits-width version error_level)))

(define (data-encode content #:version version #:mode mode #:error_level error_level)
  (let ([character_count #f]
        [mode_indicator #f]
        [character_count_indicator #f]
        [capacity_count #f]
        [encoded_data #f]
        [encoded_data_stage1 #f]
        [encoded_data_stage2 #f]
        [encoded_data_stage3 #f]
        [encoded_data_stage4 #f])
    
    (trace (format "content:~a\n" content) 2)
    (set! character_count (string-length content))
    (trace (format "character_count:~a\n" character_count) 2)

    (set! capacity_count (get-required-bits-width version error_level))
    (trace (format "capacity_count:~a\n" capacity_count) 1)

    (set! mode_indicator (get-mode-indicator mode))
    (trace (format "mode_indicator:~a\n" mode_indicator) 2)

    (set! character_count_indicator (get-character-count-indicator character_count version mode))
    (trace (format "character_count_indicator:~a\n" character_count_indicator) 2)

    (cond
     [(string=? mode "A")
      (set! encoded_data (encode-a content))]
     [(string=? mode "B")
      (set! encoded_data (encode-b content))]
     [(string=? mode "N")
      (set! encoded_data (encode-n content))])

    (trace (format "encoded_data:~a\n" (cut-string encoded_data)) 2)

    (trace (format "encoded_data_length:~a\n" (string-length encoded_data)) 2)

    ;; stage1: data origin
    (trace (format "mode_indicator:~a character_count_indicator:~a\n" mode_indicator character_count_indicator) 2)
    (set! encoded_data_stage1 (string-append mode_indicator character_count_indicator encoded_data))
    (trace (format "encoded_data_stage1:[~a]~a\n" (string-length encoded_data_stage1) encoded_data_stage1) 2)

    ;; stage2: add terminator
    (set! encoded_data_stage2 (add-terminator encoded_data_stage1 capacity_count))
    (trace (format "encoded_data_added_terminator:[~a]~a\n" (string-length encoded_data_stage2) encoded_data_stage2) 2)

    ;; stage3: add multiple eight
    (set! encoded_data_stage3 (add-multi-eight encoded_data_stage2))
    (trace (format "encoded_data_added_multiple_eight:[~a]~a\n" (string-length encoded_data_stage3) encoded_data_stage3) 2)

    ;; stage4: repeat padding
    (set! encoded_data_stage4 (repeat-right-pad-string encoded_data_stage3 capacity_count "1110110000010001"))
    (trace (format "encoded_data_added_repeat_padding:[~a]~a\n" (string-length encoded_data_stage4) encoded_data_stage4) 2)

    (trace (format "encoded_data_length:~a\n" (string-length encoded_data_stage4)) 2)

    encoded_data_stage4))
