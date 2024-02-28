#lang racket

(require "../../../share/canvas.rkt")
(require "../../../share/func.rkt")
(require "../../../share/finder-pattern.rkt")

(provide (contract-out
          [draw-finder-pattern (-> CANVAS? void?)]
          ))

(define (draw-finder-pattern canvas)
  (for-each
   (lambda (start_point)
     (for-each
      (lambda (point_pair)
        (add-point point_pair "1" "finder" points_map type_map))
      (transform-points-list (first (get-finder-pattern)) start_point))

     (for-each
      (lambda (point_pair)
        (add-point point_pair "0" "finder" points_map type_map))
      (transform-points-list (second (get-finder-pattern)) start_point))

     (for-each
      (lambda (point_pair)
        (add-point point_pair "1" "finder" points_map type_map))
      (transform-points-list (third (get-finder-pattern)) start_point)))
   (locate-finder-pattern modules)))
