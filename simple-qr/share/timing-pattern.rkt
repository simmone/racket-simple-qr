#lang racket

(require "func.rkt")

(provide (contract-out
          [locate-timing-pattern-joints (-> exact-nonnegative-integer? list?)]
          [get-timing-pattern-points (-> exact-nonnegative-integer? (listof list?))]
          ))

(define (locate-timing-pattern-joints modules)
  (let ([joint (+ 9 (- modules 16 1))])
    (list (list (cons 9  7) (cons joint 7))
          (list (cons 7  9) (cons 7 joint)))))

(define (get-timing-pattern-points modules)
  (let* ([joints (locate-timing-pattern-joints modules)]
         [vertical_joints (car joints)]
         [horizontal_joints (cadr joints)]
         [horizontal_start_point (first horizontal_joints)]
         [horizontal_end_point (second horizontal_joints)]
         [vertical_start_point (first vertical_joints)]
         [vertical_end_point (second vertical_joints)]
         [horizontal_points (get-points-between horizontal_start_point horizontal_end_point #:direction 'horizontal)]
         [vertical_points (get-points-between vertical_start_point vertical_end_point #:direction 'vertical)])
    (list horizontal_points vertical_points)))
