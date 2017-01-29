#lang racket

(require racket/draw)

(require "../func/func.rkt")
(require "../../../share/alignment-pattern.rkt")

(provide (contract-out
          [draw-alignment-pattern (-> exact-nonnegative-integer? hash? hash? void?)]
          ))

(define (draw-alignment-pattern version points_map type_map)
  (for-each
   (lambda (center_point)
     (let ([alignment_points (fill-alignment-pattern-points center_point)])
       (for-each
        (lambda (point_pair)
          (add-point point_pair "1" "alignment" points_map type_map))
        (first alignment_points))

       (for-each
        (lambda (point_pair)
          (add-point point_pair "0" "alignment" points_map type_map))
        (second alignment_points))

       (for-each
        (lambda (point_pair)
          (add-point point_pair "1" "alignment" points_map type_map))
        (third alignment_points))))
     (get-alignment-pattern-center-points version points_map type_map)))
  




