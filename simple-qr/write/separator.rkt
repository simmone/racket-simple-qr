#lang racket

(require "../share/lib.rkt"
         "../share/finder-pattern.rkt"
         "../share/separator.rkt"
         "../share/qr.rkt")

(provide (contract-out
          [draw-separator (-> QR? void?)]
          ))

(define (draw-separator qr)
  (let* ([finder_pattern_start_points (locate-finder-pattern (QR-modules qr))]
         [top_left_point (first finder_pattern_start_points)]
         [top_right_point (second finder_pattern_start_points)]
         [bottom_left_point (third finder_pattern_start_points)]
         [new_top_right_point (cons (car top_right_point) (sub1 (cdr top_right_point)))]
         [new_bottom_left_point (cons (sub1 (car bottom_left_point)) (cdr bottom_left_point))])

    (for-each
     (lambda (point)
       (add-point point 0 "separator" qr))
     (transform-points-list (first (get-separator)) top_left_point))

     (for-each
      (lambda (point)
        (add-point point 0 "separator" qr))
      (transform-points-list (second (get-separator)) new_top_right_point))

     (for-each
      (lambda (point)
        (add-point point 0 "separator" qr))
      (transform-points-list (third (get-separator)) new_bottom_left_point))
     ))
