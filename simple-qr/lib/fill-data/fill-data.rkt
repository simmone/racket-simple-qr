#lang racket

(provide (contract-out 
          [snake-modules (->* (exact-nonnegative-integer?) (#:skip_points_hash hash?) list?)]
          [draw-data (-> any/c
                         exact-nonnegative-integer?
                         list?
                         list?
                         hash?
                         void?)]
          ))

(require "../func/func.rkt")

(define (draw-data dc module_width data_list trace_list points_exists_map)
  (let loop ([loop_data_list data_list]
             [loop_trace_list trace_list])
    (when (and (not (null? loop_data_list)) (not (null? loop_trace_list)))
          (let ([bit_data (car loop_data_list)]
                [point_pair (car loop_trace_list)])
            (if (char=? bit_data #\1)
                (draw-module dc "black" (locate-brick module_width point_pair) module_width)
                (draw-module dc "white" (locate-brick module_width point_pair) module_width)))
          (loop (cdr loop_data_list) (cdr loop_trace_list)))))

(define (snake-modules modules #:skip_points_hash [skip_hash (make-hash)])
  (let ([start_point (cons modules modules)]
        [end_point (cons modules 1)])
    (reverse
     (let loop ([point start_point]
                [current_move 'up_left]
                [result_list '()])

       (if (and (not (null? result_list)) (equal? (car result_list) end_point))
           (if (hash-has-key? skip_hash (car result_list))
               (cdr result_list)
               result_list)
           (begin
             (if (and (not (null? result_list)) (hash-has-key? skip_hash (car result_list)))
                 (loop point current_move (cdr result_list))
                 (begin
                   (when (= (cdr point) 7)
                         (set! point (cons (car point) (- (cdr point) 1))))

                   (let ([next_point #f]
                         [next_move #f])
                     (cond
                      [(equal? current_move 'up_left)
                       (begin
                         (set! next_point (cons (car point) (sub1 (cdr point))))
                         (set! next_move 'up_right))]
                      [(equal? current_move 'up_right)
                       (begin
                         (set! next_point (cons (sub1 (car point)) (add1 (cdr point))))
                         (set! next_move 'up_left))]
                      [(equal? current_move 'down_left)
                       (begin
                         (set! next_point (cons (car point) (sub1 (cdr point))))
                         (set! next_move 'down_right))]
                      [(equal? current_move 'down_right)
                       (begin
                         (set! next_point (cons (add1 (car point)) (add1 (cdr point))))
                         (set! next_move 'down_left))])

                     (if (not (in-range? next_point modules))
                         (cond
                          [(equal? current_move 'up_right)
                           (loop (cons 1 (sub1 (cdr point))) 'down_left (cons point result_list))]
                          [(equal? current_move 'down_right)
                           (loop (cons modules (sub1 (cdr point))) 'up_left (cons point result_list))])
                         (loop next_point next_move (cons point result_list))))))))))))

(define (in-range? point modules)
  (and (>= (car point) 1) (<= (car point) modules) (>= (cdr point) 1) (<= (cdr point) modules)))
