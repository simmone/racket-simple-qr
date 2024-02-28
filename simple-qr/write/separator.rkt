#lang racket

(require "func/write-func.rkt")
(require "../share/func.rkt")
(require "../share/separator.rkt")
(require "../share/point.rkt")
(require "../share/qr.rkt")

(provide (contract-out
          [draw-separator (-> QR? void?)]
          ))

(define (draw-separator qr)
  (let* ([finder_pattern_start_points (locate-finder-pattern modules)]
         [top_left_point (first finder_pattern_start_points)]
         [top_right_point (second finder_pattern_start_points)]
         [bottom_left_point (third finder_pattern_start_points)]
         [new_top_right_point (cons (car top_right_point) (sub1 (cdr top_right_point)))]
         [new_bottom_left_point (cons (sub1 (car bottom_left_point)) (cdr bottom_left_point))])

    (for-each
     (lambda (point_pair)
       (add-point point_pair "0" "separator" points_map type_map))
     (transform-points-list (first (get-separator)) top_left_point))

     (for-each
      (lambda (point_pair)
        (add-point point_pair "0" "separator" points_map type_map))
      (transform-points-list (second (get-separator)) new_top_right_point))

     (for-each
      (lambda (point_pair)
        (add-point point_pair "0" "separator" points_map type_map))
      (transform-points-list (third (get-separator)) new_bottom_left_point))
     ))
