#lang racket

(require "../func/func.rkt")

(provide (contract-out
          [draw-finder-pattern (-> exact-nonnegative-integer? hash? hash? void?)]
          ))

(define *finder_pattern_points*
  '(
    ((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6) (1 . 7)
     (2 . 1)                                         (2 . 7)
     (3 . 1)                                         (3 . 7)
     (4 . 1)                                         (4 . 7)
     (5 . 1)                                         (5 . 7)
     (6 . 1)                                         (6 . 7)
     (7 . 1) (7 . 2) (7 . 3) (7 . 4) (7 . 5) (7 . 6) (7 . 7))
    (        (2 . 2) (2 . 3) (2 . 4) (2 . 5) (2 . 6)
             (3 . 2)                         (3 . 6)
             (4 . 2)                         (4 . 6)
             (5 . 2)                         (5 . 6)
             (6 . 2) (6 . 3) (6 . 4) (6 . 5) (6 . 6))
    (                (3 . 3) (3 . 4) (3 . 5)
                     (4 . 3) (4 . 4) (4 . 5)
                     (5 . 3) (5 . 4) (5 . 5))))

(define (draw-finder-pattern modules points_map type_map)
  (for-each
   (lambda (start_point)
     (for-each
      (lambda (point_pair)
        (add-point point_pair "1" "finder" points_map type_map))
      (transform-points-list (first *finder_pattern_points*) start_point))

     (for-each
      (lambda (point_pair)
        (add-point point_pair "0" "finder" points_map type_map))
      (transform-points-list (second *finder_pattern_points*) start_point))

     (for-each
      (lambda (point_pair)
        (add-point point_pair "1" "finder" points_map type_map))
      (transform-points-list (third *finder_pattern_points*) start_point)))
   (locate-finder-pattern modules)))
