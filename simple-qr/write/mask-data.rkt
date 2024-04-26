#lang racket

(provide (contract-out
          [mask-func (-> list? exact-nonnegative-integer? list?)]
          [split-matrix (-> exact-nonnegative-integer? list?)]
          [mask-condition1 (-> list? exact-nonnegative-integer?)]
          [mask-on-condition1 (-> exact-nonnegative-integer? hash? exact-nonnegative-integer?)]
          [mask-on-condition2 (-> hash? exact-nonnegative-integer?)]
          [mask-condition3 (-> list? exact-nonnegative-integer?)]
          [mask-on-condition3 (-> exact-nonnegative-integer? hash? exact-nonnegative-integer?)]
          [mask-on-condition4 (-> hash? exact-nonnegative-integer?)]
          ))

(define (get-mask-proc mask)
  (let ([mask_hash
         (hash
          0 (lambda (row column) (= (modulo (+ row column) 2) 0))
          1 (lambda (row column) (= (modulo row 2) 0))
          2 (lambda (row column) (= (modulo column 3) 0))
          3 (lambda (row column) (= (modulo (+ row column) 3) 0))
          4 (lambda (row column) (= (modulo (+ (floor (/ row 2)) (floor (/ column 3))) 2) 0))
          5 (lambda (row column) (= (+ (modulo (* row column) 2) (modulo (* row column) 3)) 0))
          6 (lambda (row column) (= (modulo (+ (modulo (* row column) 2) (modulo (* row column) 3)) 2) 0))
          7 (lambda (row column) (= (modulo (+ (modulo (+ row column) 2) (modulo (* row column) 3)) 2) 0))
          )])
    (hash-ref mask_hash mask)))

(define (mask-func data_list mask_number)
  (let ([mask-lb (get-mask-proc mask_number)])
    (reverse
     (let loop ([loop_list data_list]
                [result_list '()])
       (if (not (null? loop_list))
           (let ([point_pair (caar loop_list)]
                 [bit (cdar loop_list)])
             (loop
              (cdr loop_list)
              (cons (cons point_pair (if (mask-lb (car point_pair) (cdr point_pair)) (switch-bit bit) bit)) result_list)))
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

(define (mask-on-condition2 points_map)
  (let loop ([loop_list (hash-keys points_map)]
             [sum 0])
    (if (not (null? loop_list))
        (let* ([row (caar loop_list)]
               [col (cdar loop_list)]
               [val (hash-ref points_map (car loop_list))]
               [point2 (cons row (add1 col))]
               [point3 (cons (add1 row) col)]
               [point4 (cons (add1 row) (add1 col))])
          (if (and
               (hash-has-key? points_map point2) (string=? (hash-ref points_map point2) val)
               (hash-has-key? points_map point3) (string=? (hash-ref points_map point3) val)
               (hash-has-key? points_map point4) (string=? (hash-ref points_map point4) val))
              (loop (cdr loop_list) (+ sum 3))
              (loop (cdr loop_list) sum)))
        sum)))

(define (mask-condition3 row)
  (let ([row_str (foldr string-append "" row)])
    (if
     (or
      (regexp-match #rx"10111010000" row_str)
      (regexp-match #rx"00001011101" row_str))
     40
     0)))

(define (mask-on-condition3 modules points_map)
  (foldr 
   + 0 
   (map
    (lambda (row)
      (mask-condition3 row))
    (map
     (lambda (point_row)
       (map
        (lambda (point)
          (hash-ref points_map point))
        point_row))
     (split-matrix modules)))))

(define (mask-on-condition4 points_map)
  (let ([sum_count (hash-count points_map)]
        [dark_count (foldr + 0 (map (lambda (val) (string->number val)) (hash-values points_map)))]
        [bili #f]
        [low_val #f]
        [high_val #f]
        [low_result #f]
        [high_result #f])
    (set! bili (* (/ dark_count sum_count) 100))
    
    (set! low_val (* (floor (/ bili 5)) 5))
    
    (set! high_val (* (ceiling (/ bili 5)) 5))

    (set! low_result (/ (abs (- low_val 50)) 5))
    (set! high_result (/ (abs (- high_val 50)) 5))

    (if (< low_result high_result)
        (* low_result 10)
        (* high_result 10))))
