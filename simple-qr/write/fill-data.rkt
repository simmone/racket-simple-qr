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
                (add-raw-point point_pair 1 'data qr)
                (add-raw-point point_pair 0 'data qr))
            )
          (if (null? loop_data_list)
              (loop '() (cdr loop_trace_list))
              (loop (cdr loop_data_list) (cdr loop_trace_list))))))

(define (get-data-socket-list modules #:skip_points_hash [skip_hash (make-hash)])
  (let ([start_point (add-quiet-zone-offset (cons (sub1 modules) (sub1 modules)))]
        [end_point (add-quiet-zone-offset (cons (sub1 modules) 0))])
    (let loop ([loop_point start_point]
               [current_move 'up_left]
               [result_list '()])

      (let ([next_point #f]
            [next_move #f])
        (cond
         [(equal? current_move 'up_left)
          (set! next_point (cons (car loop_point) (sub1 (cdr loop_point))))
          (set! next_move 'up_right)]
         [(equal? current_move 'up_right)
          (set! next_point (cons (sub1 (car loop_point)) (add1 (cdr loop_point))))
          (set! next_move 'up_left)]
         [(equal? current_move 'down_left)
          (set! next_point (cons (car loop_point) (sub1 (cdr loop_point))))
          (set! next_move 'down_right)]
         [(equal? current_move 'down_right)
          (set! next_point (cons (add1 (car loop_point)) (add1 (cdr loop_point))))
          (set! next_move 'down_left)])

        (when (not (in-range? next_point modules))
          (let ([next_col (sub1 (cdr loop_point))])

            ;; if next column is vertical timing pattern, skip
            (when (= next_col (+ QUIET_ZONE_BRICKS 6))
              (set! next_col (sub1 next_col)))

            (cond
             [(equal? current_move 'up_right)
              (set! next_point (cons QUIET_ZONE_BRICKS next_col))
              (set! next_move 'down_left)]
             [(equal? current_move 'down_right)
              (set! next_point (cons (sub1 (+ QUIET_ZONE_BRICKS modules)) next_col))
              (set! next_move 'up_left)])))

        (if (hash-has-key? skip_hash loop_point)
            (if (equal? next_point end_point)
                (reverse result_list)
                (loop next_point next_move result_list))
            (if (equal? next_point end_point)
                (reverse (cons loop_point result_list))
                (loop next_point next_move (cons loop_point result_list))))))))

(define (in-range? point modules)
  (and (>= (car point) QUIET_ZONE_BRICKS) (< (car point) (+ QUIET_ZONE_BRICKS modules)) (>= (cdr point) QUIET_ZONE_BRICKS) (< (cdr point) (+ QUIET_ZONE_BRICKS modules))))

