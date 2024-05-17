#lang racket

(require racket/draw
         "../share/lib.rkt"
         "../share/qr.rkt")

(provide (contract-out
          [draw-timing-pattern (-> QR? void?)]
          [locate-timing-pattern-joints (-> natural? list?)]
          [get-timing-pattern-points (-> natural? (listof list?))]
          ))

(define (draw-timing-pattern qr)
  (let ([timing_pattern_points (get-timing-pattern-points (QR-modules qr))])

    (let loop ([points (first timing_pattern_points)]
               [index 0])
      (when (not (null? points))
            (let ([point (car points)])
              (if (even? index)
                  (add-point point 1 'timing qr)
                  (add-point point 0 'timing qr)))
            (loop (cdr points) (add1 index))))

    (let loop ([points (second timing_pattern_points)]
               [index 0])
      (when (not (null? points))
            (let ([point (car points)])
              (if (even? index)
                  (add-point point 1 'timing qr)
                  (add-point point 0 'timing qr)))
            (loop (cdr points) (add1 index))))))

(define (locate-timing-pattern-joints modules)
  (let ([joint (- modules 9)])
    (list
     (list (cons 6  8) (cons 6 joint))
     (list (cons 8  6) (cons joint 6)))))

(define (get-timing-pattern-points modules)
  (let* ([joints (locate-timing-pattern-joints modules)]
         [horizontal_joints (car joints)]
         [vertical_joints (cadr joints)]
         [horizontal_start_point (first horizontal_joints)]
         [horizontal_end_point (second horizontal_joints)]
         [vertical_start_point (first vertical_joints)]
         [vertical_end_point (second vertical_joints)]
         [horizontal_points (get-points-between horizontal_start_point horizontal_end_point #:direction 'horizontal)]
         [vertical_points (get-points-between vertical_start_point vertical_end_point #:direction 'vertical)])
    (list horizontal_points vertical_points)))
