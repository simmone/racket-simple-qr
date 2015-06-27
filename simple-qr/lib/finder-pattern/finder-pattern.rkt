#lang racket

(require "../func/func.rkt")

(provide (contract-out
          [draw-finder-pattern (-> any/c
                                   exact-nonnegative-integer?
                                   exact-nonnegative-integer?
                                   hash?
                                   void?)]
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

(define (draw-finder-pattern dc modules module_width points_map)
  (for-each
   (lambda (start_point)
     (for-each
      (lambda (point_pair)
        (hash-set! points_map point_pair '("1" . "finder")))
      (transform-points-list (first *finder_pattern_points*) start_point))

     (for-each
      (lambda (point_pair)
        (hash-set! points_map point_pair '("0" . "finder")))
      (transform-points-list (second *finder_pattern_points*) start_point))

     (for-each
      (lambda (point_pair)
        (hash-set! points_map point_pair '("1" . "finder")))
      (transform-points-list (third *finder_pattern_points*) start_point)))
   (locate-finder-pattern modules)))
