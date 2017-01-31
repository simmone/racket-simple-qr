#lang racket

(require racket/draw)

(provide (contract-out
          [get-points-between (-> pair? pair? #:direction (or/c 'horizontal 'vertical) list?)]
          [get-points (-> (listof list?) (listof pair?) any)]
          [get-onezero-bits (-> exact-nonnegative-integer? string?)]
          ))

(define (get-onezero-bits width)
  (let loop ([loop_width width]
             [result_list '()]
             [bit "1"])
    (if (> loop_width 0)
        (loop (sub1 loop_width) (cons bit result_list) (if (string=? bit "1") "0" "1"))
        (foldr (lambda (a b) (string-append a b)) "" (reverse result_list)))))

(define (get-points matrix trace_list)
  (let loop ([loop_list trace_list]
             [result_list '()])
    (if (not (null? loop_list))
        (loop (cdr loop_list) (cons (list-ref (list-ref matrix (sub1 (car (car loop_list)))) (sub1 (cdr (car loop_list)))) result_list))
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
