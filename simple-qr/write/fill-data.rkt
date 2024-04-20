#lang racket

(require "../share/qr.rkt")

(provide (contract-out 
          [draw-data (-> string? list? QR? void?)]
          [get-data-socket-list (->* (natural?) (#:skip_points_hash hash?) list?)]
          ))

(define (draw-data bits_string trace_list qr)
  (let loop ([loop_data_list (string->list bits_string)]
             [loop_trace_list trace_list])
    (when (not (null? loop_trace_list))
          (let ([bit_data (if (null? loop_data_list) #\0 (car loop_data_list))]
                [point_pair (car loop_trace_list)])
            (if (char=? bit_data #\1)
                (add-point point_pair 1 'data qr)
                (add-point point_pair 0 'data qr)))
          (if (null? loop_data_list)
              (loop '() (cdr loop_trace_list))
              (loop (cdr loop_data_list) (cdr loop_trace_list))))))

(define (get-data-socket-list modules #:skip_points_hash [skip_hash (make-hash)])
  (let ([start_point (add-quiet-zone-offset (cons modules modules))]
        [end_point (add-quiet-zone-offset (cons modules 0))])
     (let loop ([point start_point]
                [current_move 'up_left]
                [result_list '()])

       (let ([next_point #f]
             [next_move #f])
         (cond
          [(equal? current_move 'up_left)
           (set! next_point (cons (car point) (sub1 (cdr point))))
           (set! next_move 'up_right)]
          [(equal? current_move 'up_right)
           (set! next_point (cons (sub1 (car point)) (add1 (cdr point))))
           (set! next_move 'up_left)]
          [(equal? current_move 'down_left)
           (set! next_point (cons (car point) (sub1 (cdr point))))
           (set! next_move 'down_right)]
          [(equal? current_move 'down_right)
           (set! next_point (cons (add1 (car point)) (add1 (cdr point))))
           (set! next_move 'down_left)])
         
         (if (equal? point end_point)
             (reverse result_list)
             (if (hash-has-key? skip_hash point)
                 (loop next_point next_move result_list)
                 (if (not (in-range? next_point modules))
                     (cond
                      [(equal? current_move 'up_right)
                       (loop (add-quiet-zone-offset (cons 0 (cdr point))) 'down_left (cons point result_list))]
                      [(equal? current_move 'down_right)
                       (loop (add-quiet-zone-offset (cons (sub1 modules) (cdr point))) 'up_left (cons point result_list))]
                      [else
                       (printf "point:~a, current_move:~a, next_point:~a, next_point is in range:~a\n" point current_move next_point (in-range? next_point modules))])
                     (loop next_point next_move (cons point result_list)))))))))

(define (in-range? point modules)
  (and (>= (car point) 0) (< (car point) (+ modules QUIET_ZONE_BRICKS)) (>= (cdr point) 0) (< (cdr point) (+ modules QUIET_ZONE_BRICKS))))

