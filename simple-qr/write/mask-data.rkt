#lang racket

(require "../share/qr.rkt"
         "../share/draw/matrix.rkt"
         "../share/lib.rkt")

(provide (contract-out
          [mask-func (-> list? natural? list?)]
          [get-all-data-rows (-> natural? list?)]
          [get-all-data-cols (-> natural? list?)]
          [mask-condition1 (-> list? natural?)]
          [mask-on-condition1 (-> natural? hash? natural?)]
          [mask-on-condition2 (-> hash? natural?)]
          [mask-condition3 (-> list? natural?)]
          [mask-on-condition3 (-> natural? hash? natural?)]
          [mask-on-condition4 (-> natural? hash? natural?)]
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
              (cons
               (cons
                point_pair
                (if (mask-lb (- (car point_pair) QUIET_ZONE_BRICKS) (- (cdr point_pair) QUIET_ZONE_BRICKS)) (switch-bit bit) bit))
               result_list)))
           result_list)))))

(define (switch-bit bit)
  (if (= bit 1)
      0
      1))

(define (get-all-data-rows modules)
  (let loop-row ([row 0]
                 [result_list '()])
    (if (< row modules)
        (loop-row (add1 row)
                  (cons
                   (let loop-col ([col 0]
                                  [row_list '()])
                     (if (< col modules)
                         (loop-col (add1 col) (cons (add-quiet-zone-offset (cons row col)) row_list))
                         (reverse row_list)))
                   result_list))
        (reverse result_list))))

(define (get-all-data-cols modules)
  (let loop-row ([col 0]
                 [result_list '()])
    (if (< col modules)
        (loop-row (add1 col)
                  (cons
                   (let loop-col ([row 0]
                                  [col_list '()])
                     (if (< row modules)
                         (loop-col (add1 row) (cons (add-quiet-zone-offset (cons row col)) col_list))
                         (reverse col_list)))
                   result_list))
        (reverse result_list))))

(define (mask-condition1 line)
  (let ([sum_count 0])
    (let loop ([loop_list line]
               [last_item -1]
               [single_count 0])
      (cond
       [(= single_count 5)
        (set! sum_count (+ sum_count 3))]
       [(> single_count 5)
        (set! sum_count (add1 sum_count))])

      (when (not (null? loop_list))
            (if (= (car loop_list) last_item)
                (loop (cdr loop_list) (car loop_list) (add1 single_count))
                (loop (cdr loop_list) (car loop_list) 1))))
    sum_count))

(define (mask-on-condition1 modules points_map)
  (foldr
   + 0
   (map
    (lambda (line)
      (mask-condition1 line))
    (map
     (lambda (point_line)
       (map
        (lambda (point)
          (hash-ref points_map point))
        point_line))
     (append
      (get-all-data-rows modules)
      (get-all-data-cols modules))))))

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
               (hash-has-key? points_map point2) (= (hash-ref points_map point2) val)
               (hash-has-key? points_map point3) (= (hash-ref points_map point3) val)
               (hash-has-key? points_map point4) (= (hash-ref points_map point4) val))
              (loop (cdr loop_list) (+ sum 3))
              (loop (cdr loop_list) sum)))
        sum)))

(define (mask-condition3 row)
  (let ([row_str (foldr string-append "" (map (lambda (val) (format "~a" val)) row))])
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
     (append
      (get-all-data-rows modules)
      (get-all-data-cols modules))))))

(define (mask-on-condition4 modules points_map)
  (let* ([qr_area_points
          (get-points-between
           (cons QUIET_ZONE_BRICKS QUIET_ZONE_BRICKS)
           (cons (- (+ modules QUIET_ZONE_BRICKS) 1) (- (+ modules QUIET_ZONE_BRICKS) 1))
           #:direction 'cross)]
         [sum_count (length qr_area_points)]
         [dark_count (foldr + 0
                            (map
                             (lambda (point)
                               (hash-ref points_map point))
                             qr_area_points))]
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

    (* (min low_result high_result) 10)))
