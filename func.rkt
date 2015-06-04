#lang racket

(require racket/draw)

(require "define.rkt")

(provide (contract-out
          [locate-brick (-> exact-nonnegative-integer?
                            pair?
                            pair?)]
          [white-block (-> any/c
                           pair?
                           pair?
                           void?)]
          [black-block (-> any/c
                           pair?
                           pair?
                           void?)]
          [locate-finder-pattern (->
                                  exact-nonnegative-integer?
                                  exact-nonnegative-integer?
                                  (values
                                   pair?
                                   pair?
                                   pair?))]
          [transform-points-list (-> list?
                                     pair?
                                     list?)]
          ))

(define (white-block dc place_pair module_width)
  (send dc set-pen "white" 1 'solid)
  (send dc set-brush "white" 'solid)

  (send dc draw-rectangle (car place_pair) (cdr place_pair) module_width module_width))

(define (black-block dc place_pair module_width)
  (send dc set-pen "black" 1 'solid)
  (send dc set-brush "black" 'solid)

  (send dc draw-rectangle (car place_pair) (cdr place_pair) module_width module_width))

(define (locate-brick module_width place_pair)
  (cons (+ module_width (* (sub1 (cdr place_pair)) module_width))
        (+ module_width (* (sub1 (car place_pair)) module_width))))

(define (locate-finder-pattern version module_width)
  (values
   (cons module_width module_width)
   (cons (- (* version module_width) module_width (* 7 module_width)) module_width)
   (cons module_width (- (* version module_width) module_width (* 7 module_width)))))

(define (transform-points-list points_list start_point_pair)
  (map
   (lambda (point)
     (cons (+ (car start_point_pair) (sub1 (car point))) (+ (cdr start_point_pair) (sub1 (cdr point)))))
    points_list))

(define (draw-finder-pattern dc module_width)
  (for-each
   (lambda (point_pair)
     (black-block dc (locate-brick module_width point_pair) module_width))
   (first *finder_pattern_points*))

  (for-each
   (lambda (point_pair)
     (white-block dc (locate-brick module_width point_pair) module_width))
   (second *finder_pattern_points*))

  (for-each
   (lambda (point_pair)
     (black-block dc (locate-brick module_width point_pair) module_width))
   (third *finder_pattern_points*))
  )

