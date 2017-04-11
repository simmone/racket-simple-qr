#lang racket

(require "../func/func.rkt")
(require "../../../share/finder-pattern.rkt")

(provide (contract-out
          [draw-finder-pattern (-> exact-nonnegative-integer? hash? hash? void?)]
          ))

(define (draw-finder-pattern modules points_map type_map)
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
