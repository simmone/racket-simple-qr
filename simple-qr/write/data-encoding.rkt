#lang racket

(require "../share/lib.rkt"
         "data-group.rkt"
        racket/format)

(provide (contract-out
          [get-character-bit-width (-> natural? (or/c 'A 'B 'K 'N) natural?)]
          [encode-b (-> string? string?)]
          [encode-n (-> string? string?)]
          [string-split (-> string? natural? list?)]
          [encode-a (-> string? string?)]
          ))

(define (get-character-bit-width version mode)
  (cond
   [(and (>= version 1) (<= version 9))
    (cond
     [(eq? mode 'N)
      10]
     [(eq? mode 'A)
      9]
     [(eq? mode 'B)
      8]
     [(eq? mode 'K)
      8])]
   [(and (>= version 10) (<= version 26))
    (cond
     [(eq? mode 'N)
      12]
     [(eq? mode 'A)
      11]
     [(eq? mode 'B)
      16]
     [(eq? mode 'K)
      10])]
   [(and (>= version 27) (<= version 40))
    (cond
     [(eq? mode 'N)
      14]
     [(eq? mode 'A)
      13]
     [(eq? mode 'B)
      16]
     [(eq? mode 'K)
      12])]))

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

(define *alphanumeric_num_table*
  '#hash(
         ("0" . 0)
         ("1" . 1)
         ("2" . 2)
         ("3" . 3)
         ("4" . 4)
         ("5" . 5)
         ("6" . 6)
         ("7" . 7)
         ("8" . 8)
         ("9" . 9)
         ("A" . 10)
         ("B" . 11)
         ("C" . 12)
         ("D" . 13)
         ("E" . 14)
         ("F" . 15)
         ("G" . 16)
         ("H" . 17)
         ("I" . 18)
         ("J" . 19)
         ("K" . 20)
         ("L" . 21)
         ("M" . 22)
         ("N" . 23)
         ("O" . 24)
         ("P" . 25)
         ("Q" . 26)
         ("R" . 27)
         ("S" . 28)
         ("T" . 29)
         ("U" . 30)
         ("V" . 31)
         ("W" . 32)
         ("X" . 33)
         ("Y" . 34)
         ("Z" . 35)
         (" " . 36)
         ("$" . 37)
         ("%" . 38)
         ("*" . 39)
         ("+" . 40)
         ("-" . 41)
         ("." . 42)
         ("/" . 43)
         (":" . 44)))

(define (get-alphanumeric-num mode)
  (hash-ref *alphanumeric_num_table* mode))
