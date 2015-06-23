#lang racket

(require "../func/func.rkt")

(provide (contract-out
          [draw-version-information (-> any/c
                                exact-nonnegative-integer?
                                exact-nonnegative-integer?
                                exact-nonnegative-integer?
                                hash?
                                void?)]
          ))

(define *version_points*
  '(
    ((1 . 1) (1 . 2) (1 . 3)
     (2 . 1) (2 . 2) (2 . 3)
     (3 . 1) (3 . 2) (3 . 3)
     (4 . 1) (4 . 2) (4 . 3)
     (5 . 1) (5 . 2) (5 . 3)
     (6 . 1) (6 . 2) (6 . 3))
    ((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6)
     (2 . 1) (2 . 2) (2 . 3) (2 . 4) (2 . 5) (2 . 6)
     (3 . 1) (3 . 2) (3 . 3) (3 . 4) (3 . 5) (3 . 6))
    ))

(define (draw-version-information dc version modules module_width points_exists_map)
  (let* ([finder_pattern_start_points (locate-finder-pattern modules)]
         [bottom_left_point (second finder_pattern_start_points)]
         [top_right_point (third finder_pattern_start_points)]
         [new_bottom_left_point (cons (- (car bottom_left_point) 4) (cdr bottom_left_point))]
         [new_top_right_point (cons (car top_right_point) (- (cdr top_right_point) 4))])
    (for-each
     (lambda (point_pair)
       (draw-module dc "blue" (locate-brick module_width point_pair) module_width)
       (hash-set! points_exists_map point_pair "version_information"))
     (transform-points-list (first *version_points*) new_top_right_point))

     (for-each
      (lambda (point_pair)
        (draw-module dc "blue" (locate-brick module_width point_pair) module_width)
        (hash-set! points_exists_map point_pair "version_information"))
      (transform-points-list (second *version_points*) new_bottom_left_point))))
