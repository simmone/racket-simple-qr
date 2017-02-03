#lang racket

(require racket/draw)

(provide (contract-out
          [get-points-between (-> pair? pair? #:direction (or/c 'horizontal 'vertical) list?)]
          [get-points (-> (listof list?) (listof pair?) any)]
          [get-unmask-points (-> (listof list?) (listof pair?) any)]
          ))

(define (get-points matrix trace_list)
  (let loop ([loop_list trace_list]
             [result_list '()])
    (if (not (null? loop_list))
        (let* ([i (sub1 (caar loop_list))]
               [j (sub1 (cdar loop_list))]
               [val (list-ref (list-ref matrix i) j)])
          (loop (cdr loop_list) (cons val result_list)))
        (reverse result_list))))

(define (get-unmask-points matrix trace_list)
  (let loop ([loop_list trace_list]
             [result_list '()])
    (if (not (null? loop_list))
        (let* ([i (sub1 (caar loop_list))]
               [j (sub1 (cdar loop_list))]
               [val (list-ref (list-ref matrix i) j)])
          (loop (cdr loop_list) (cons (if (= (remainder (+ i j) 2) 0) (bitwise-xor val 1) val) result_list)))
        (reverse result_list))))

(define (get-points-between start_point end_point #:direction direction)
  (let ([is_valid? #f])
    (if (equal? direction 'horizontal)
        (when (and
               (= (car start_point) (car end_point))
               (<= (cdr start_point) (cdr end_point)))
              (set! is_valid? #t))
        (when (and
               (= (cdr start_point) (cdr end_point))
               (<= (car start_point) (car end_point)))
              (set! is_valid? #t)))
    (if (not is_valid?)
        '()
        (let ([points '()]
              [vars '()]
              [start_var (if (equal? direction 'horizontal) (cdr start_point) (car start_point))]
              [end_var (if (equal? direction 'horizontal) (cdr end_point) (car end_point))])
          (let loop ([var start_var])
            (when (<= var end_var)
                  (set! vars `(,@vars ,var))
                  (loop (add1 var))))
          (for-each
           (lambda (cor)
             (if (equal? direction 'horizontal)
                 (set! points `(,@points ,(cons (car start_point) cor)))
                 (set! points `(,@points ,(cons cor (cdr start_point))))))
           vars)
          points))))
