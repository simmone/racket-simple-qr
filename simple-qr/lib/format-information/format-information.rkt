#lang racket

(require "../func/func.rkt")

(provide (contract-out
          [draw-format-information (-> any/c
                                exact-nonnegative-integer?
                                exact-nonnegative-integer?
                                hash?
                                void?)]
          ))

(define *information_points*
  '(
    (                                                                (1 . 9)
                                                                     (2 . 9)
                                                                     (3 . 9)
                                                                     (4 . 9)
                                                                     (5 . 9)
                                                                     (6 . 9)

                                                                     (8 . 9)
     (9 . 1) (9 . 2) (9 . 3) (9 . 4) (9 . 5) (9 . 6)         (9 . 8) (9 . 9))
    (                                                                
                                                                     (2 . 9)
                                                                     (3 . 9)
                                                                     (4 . 9)
                                                                     (5 . 9)
                                                                     (6 . 9)
                                                                     (7 . 9)
                                                                     (8 . 9)
                                                                     (9 . 9))
    ((9 . 1) (9 . 2) (9 . 3) (9 . 4) (9 . 5) (9 . 6) (9 . 7) (9 . 8) (9 . 9))))

(define (draw-format-information dc modules module_width points_exists_map)
  (let* ([finder_pattern_start_points (locate-finder-pattern modules)]
         [top_left_point (first finder_pattern_start_points)]
         [bottom_left_point (second finder_pattern_start_points)]
         [top_right_point (third finder_pattern_start_points)]
         [new_bottom_left_point (cons (sub1 (car bottom_left_point)) (cdr bottom_left_point))]
         [new_bottom_point (cons (car top_right_point) (sub1 (cdr top_right_point)))])
    (for-each
     (lambda (point_pair)
       (draw-module dc "blue" (locate-brick module_width point_pair) module_width)
       (hash-set! points_exists_map point_pair "format_information"))
     (transform-points-list (first *information_points*) top_left_point))

     (for-each
      (lambda (point_pair)
        (draw-module dc "blue" (locate-brick module_width point_pair) module_width)
        (hash-set! points_exists_map point_pair "format_information"))
      (transform-points-list (second *information_points*) new_bottom_left_point))

     (for-each
      (lambda (point_pair)
        (draw-module dc "blue" (locate-brick module_width point_pair) module_width)
        (hash-set! points_exists_map point_pair "format_information"))
      (transform-points-list (third *information_points*) new_bottom_point))))
