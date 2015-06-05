#lang racket

(require racket/draw)

(require "define.rkt")

(provide (contract-out
          [locate-brick (-> exact-nonnegative-integer?
                            pair?
                            pair?)]
          [draw-module (-> (is-a?/c bitmap-dc%)
                           (or/c (is-a?/c color%) string?)
                           pair?
                           exact-nonnegative-integer?
                           void?)]
          [locate-finder-pattern (->
                                  exact-nonnegative-integer?
                                  list?)]
          [transform-points-list (-> list?
                                     pair?
                                     list?)]
          [draw-finder-pattern (-> any/c
                                   exact-nonnegative-integer?
                                   exact-nonnegative-integer?
                                   void?)]
          [draw-separator (-> any/c
                              exact-nonnegative-integer?
                              exact-nonnegative-integer?
                              void?)]
          ))

(define (draw-module dc color place_pair module_width)
  (send dc set-pen color 1 'solid)
  (send dc set-brush color 'solid)

  (send dc draw-rectangle (car place_pair) (cdr place_pair) module_width module_width))

(define (locate-brick module_width place_pair)
  (cons (* (sub1 (cdr place_pair)) module_width)
        (* (sub1 (car place_pair)) module_width)))

(define (locate-finder-pattern version)
  (list
   '(1 . 1)
   (cons (add1 (- version 7)) 1)
   (cons 1 (add1 (- version 7)))))

(define (transform-points-list points_list start_point_pair)
  (map
   (lambda (point)
     (cons (+ (car start_point_pair) (sub1 (car point))) (+ (cdr start_point_pair) (sub1 (cdr point)))))
    points_list))

(define (draw-finder-pattern dc version module_width)
  (for-each
   (lambda (start_point)
     (for-each
      (lambda (point_pair)
        (draw-module dc "black" (locate-brick module_width point_pair) module_width))
      (transform-points-list (first *finder_pattern_points*) start_point))

     (for-each
      (lambda (point_pair)
        (draw-module dc "white" (locate-brick module_width point_pair) module_width))
      (transform-points-list (second *finder_pattern_points*) start_point))

     (for-each
      (lambda (point_pair)
        (draw-module dc "black" (locate-brick module_width point_pair) module_width))
      (transform-points-list (third *finder_pattern_points*) start_point)))
   (locate-finder-pattern version)))

(define (draw-separator dc version module_width)
  (let* ([finder_pattern_start_points (locate-finder-pattern version)]
         [top_left_point (first finder_pattern_start_points)]
         [top_right_point (second finder_pattern_start_points)]
         [bottom_left_point (third finder_pattern_start_points)]
         [new_top_right_point (cons (sub1 (car top_right_point)) (cdr top_right_point))]
         [new_bottom_point (cons (car bottom_left_point) (sub1 (cdr bottom_left_point)))])
    (for-each
     (lambda (point_pair)
       (draw-module dc "white" (locate-brick module_width point_pair) module_width))
     (transform-points-list (first *separator_points*) top_left_point))

     (for-each
      (lambda (point_pair)
        (draw-module dc "white" (locate-brick module_width point_pair) module_width))
      (transform-points-list (second *separator_points*) new_top_right_point))

     (for-each
      (lambda (point_pair)
        (draw-module dc "white" (locate-brick module_width point_pair) module_width))
      (transform-points-list (third *separator_points*) new_bottom_point))))

