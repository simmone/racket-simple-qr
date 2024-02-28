#lang racket

(require racket/draw)

(provide (contract-out
          [version->modules (-> exact-nonnegative-integer? exact-nonnegative-integer?)]
          [transform-points-list (-> list? pair? list?)]
          [add-terminator (-> string? exact-nonnegative-integer? string?)]
          [add-multi-eight (-> string? string?)]
          [repeat-right-pad-string (-> string? exact-nonnegative-integer? string? string?)]
          [split-bit-string-to-decimal (-> string? list?)]
          [split-decimal-list-on-contract (-> list? list? list?)]
          [interleave-list (-> list? list?)]
          [decimal-list-to-string (-> list? string?)]
          [cut-string (-> string? list)]
          [to-message-poly (-> list? string?)]
          ))

(define (version->modules version)
  (if (and (>= version 1) (<= version 40))
      (+ 21 (* 4 (sub1 version)))
      (error "invalid version!")))

(define (transform-points-list points_list start_point_pair)
  (map
   (lambda (point)
     (cons (+ (car start_point_pair) (sub1 (car point))) (+ (cdr start_point_pair) (sub1 (cdr point)))))
   points_list))

(define (add-terminator content limit_length)
  (let* ([content_length (string-length content)]
         [gap (- limit_length content_length)])
    (if (<= gap 0)
        content
        (if (<= gap 4)
            (~a content #:min-width (+ content_length gap) #:right-pad-string "0")
            (~a content #:min-width (+ content_length 4) #:right-pad-string "0")))))

(define (add-multi-eight content)
  (let* ([content_length (string-length content)]
         [eight_length (* 8 (ceiling (/ content_length 8)))])
    (~a content #:min-width eight_length #:right-pad-string "0")))

(define (repeat-right-pad-string content limit_length pad_str)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_content content])
        (if (>= (string-length loop_content) limit_length)
            (printf "~a" loop_content)
            (let loop_inner ([inner_loop_content loop_content]
                             [pad_list (string->list pad_str)])
              (if (not (null? pad_list))
                  (if (>= (string-length inner_loop_content) limit_length)
                      (printf "~a" inner_loop_content)
                      (loop_inner (format "~a~a" inner_loop_content (car pad_list)) (cdr pad_list)))
                  (loop inner_loop_content))))))))

(define (split-bit-string-to-decimal bit_str)
  (reverse
   (let loop ([loop_str bit_str]
              [result_list '()])
     (if (not (string=? loop_str ""))
         (loop (substring loop_str 8) (cons (string->number (string-append "#b" (substring loop_str 0 8))) result_list))
         result_list))))

(define (split-decimal-list-on-contract num_list contract)
  (let ([group1_block_count (car (first contract))]
        [group1_count_per_block (cdr (first contract))]
        [group2_block_count (car (second contract))]
        [group2_count_per_block (cdr (second contract))]
        [remain_list #f])

    (list
     (let loop ([loop_list num_list]
                [loop_block_count group1_block_count]
                [loop_count group1_count_per_block]
                [temp_result_list '()]
                [result_list '()])
       (if (= loop_block_count 0)
           (begin
             (set! remain_list loop_list)
             (reverse result_list))
           (if (= loop_count 1)
               (loop (cdr loop_list) (sub1 loop_block_count) group1_count_per_block '() (cons (reverse (cons (car loop_list) temp_result_list)) result_list))
               (loop (cdr loop_list) loop_block_count (sub1 loop_count) (cons (car loop_list) temp_result_list) result_list))))

     (let loop ([loop_list remain_list]
                [loop_block_count group2_block_count]
                [loop_count group2_count_per_block]
                [temp_result_list '()]
                [result_list '()])
       (if (= loop_block_count 0)
           (reverse result_list)
           (if (= loop_count 1)
               (loop (cdr loop_list) (sub1 loop_block_count) group2_count_per_block '() (cons (reverse (cons (car loop_list) temp_result_list)) result_list))
               (loop (cdr loop_list) loop_block_count (sub1 loop_count) (cons (car loop_list) temp_result_list) result_list)))))))

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

