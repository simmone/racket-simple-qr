#lang racket

(provide (contract-out
          [mask-data (-> list? exact-nonnegative-integer? list?)]
          [split-matrix (-> exact-nonnegative-integer? list?)]
          [mask-condition1 (-> list? exact-nonnegative-integer?)]
          [mask-on-condition1 (-> exact-nonnegative-integer? hash? exact-nonnegative-integer?)]
          ))

(define *mask_proc_hash*
  (hash
   0 (lambda (row column) (= (modulo (+ row column) 2) 0))
   1 (lambda (row column) (= (modulo row 2) 0))
   2 (lambda (row column) (= (modulo column 3) 0))
   3 (lambda (row column) (= (modulo (+ row column) 3) 0))
   4 (lambda (row column) (= (modulo (+ (floor (/ row 2)) (floor (/ column 3))) 2) 0))
   5 (lambda (row column) (= (+ (modulo (* row column) 2) (modulo (* row column) 3)) 0))
   6 (lambda (row column) (= (modulo (+ (modulo (* row column) 2) (modulo (* row column) 3)) 2) 0))
   7 (lambda (row column) (= (modulo (+ (modulo (+ row column) 2) (modulo (* row column) 3)) 2) 0))
   ))

(define (mask-data data_list mask_number)
  (let ([mask-func (hash-ref *mask_proc_hash* mask_number)])
    (reverse
     (let loop ([loop_list data_list]
                [result_list '()])
       (if (not (null? loop_list))
           (let ([point_pair (caar loop_list)]
                 [bit (cdar loop_list)])
             (loop
              (cdr loop_list)
              (cons (cons point_pair (if (mask-func (car point_pair) (cdr point_pair)) (switch-bit bit) bit)) result_list)))
           result_list)))))

(define (switch-bit bit)
  (if (string=? bit "1")
      "0"
      "1"))

(define (split-matrix modules)
  `(,@(reverse
       (let loop-row ([row 1]
                      [result_list '()])
         (if (<= row modules)
             (loop-row (add1 row)
                       (cons 
                        (reverse
                         (let loop-col ([col 1]
                                        [row_list '()])
                           (if (<= col modules)
                               (loop-col (add1 col) (cons (cons row col) row_list))
                               row_list)))
                        result_list))
             result_list)))

    ,@(reverse
       (let loop-row ([col 1]
                      [result_list '()])
         (if (<= col modules)
             (loop-row (add1 col)
                       (cons 
                        (reverse
                         (let loop-col ([row 1]
                                        [col_list '()])
                           (if (<= row modules)
                               (loop-col (add1 row) (cons (cons row col) col_list))
                               col_list)))
                        result_list))
             result_list)))))

(define (mask-condition1 row)
  (let ([sum_count 0])
    (let loop ([loop_list row]
               [last_item ""]
               [single_count 0])
      (cond
       [(= single_count 5)
        (set! sum_count (+ sum_count 3))]
       [(> single_count 5)
        (set! sum_count (add1 sum_count))])

    (when (not (null? loop_list))
          (if (string=? (car loop_list) last_item)
              (loop (cdr loop_list) (car loop_list) (add1 single_count))
              (loop (cdr loop_list) (car loop_list) 1))))
    sum_count))

(define (mask-on-condition1 modules points_map)
  (foldr 
   + 0 
   (map
    (lambda (row)
      (mask-condition1 row))
    (map
     (lambda (point_row)
       (map
        (lambda (point)
          (hash-ref points_map point))
        point_row))
     (split-matrix modules)))))
         
         
