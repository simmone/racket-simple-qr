#lang racket

(require "../share/lib.rkt"
         "../share/qr.rkt"
         "../share/finder-pattern.rkt"
         )

(require racket/format)

(provide (contract-out
          [draw-format-information (-> string? QR? void?)]
          ))

(define (draw-format-information format_string qr)
  (let* ([finder_pattern_start_points (locate-finder-pattern (QR-modules qr))]
         [top_left_point (first finder_pattern_start_points)]
         [top_right_point (second finder_pattern_start_points)]
         [bottom_left_point (third finder_pattern_start_points)]
         [new_bottom_left_point (cons (sub1 (car bottom_left_point)) (cdr bottom_left_point))]
         [new_top_right_point (cons (car top_right_point) (sub1 (cdr top_right_point)))])

    (let loop ([data_list (reverse (string->list (string-append format_string format_string)))]
               [trace_list `(
                             ,@(transform-points-list (first (get-format-information)) top_left_point)
                             ,@(transform-points-list (second (get-format-information)) new_top_right_point)
                             ,@(transform-points-list (third (get-format-information)) new_bottom_left_point))])

      (when (and (not (null? data_list)) (not (null? trace_list)))
            (if (char=? (car data_list) #\0)
                (add-point (car trace_list) 0 'format qr)
                (add-point (car trace_list) 1 'format qr))
            (loop (cdr data_list) (cdr trace_list))))))

(define (get-format-information)
  '(
    (                                                                (0 . 8)
                                                                     (1 . 8)
                                                                     (2 . 8)
                                                                     (3 . 8)
                                                                     (4 . 8)
                                                                     (5 . 8)

                                                                     (7 . 8)
     (8 . 8) (8 . 7) (8 . 5) (8 . 4) (8 . 3) (8 . 2)         (8 . 1) (8 . 0))

    ((8 . 7) (8 . 6) (8 . 5) (8 . 4) (8 . 3) (8 . 2) (8 . 1) (8 . 0))

    (                                                                (1 . 8)
                                                                     (2 . 8)
                                                                     (3 . 8)
                                                                     (4 . 8)
                                                                     (5 . 8)
                                                                     (6 . 8)
                                                                     (7 . 8))
    ))

