#lang racket

(require racket/draw)

(require "func.rkt")

(provide (contract-out
          [locate-timing-pattern-joints (->
                                         exact-nonnegative-integer?
                                         list?)]
          [draw-timing-pattern (->
                                (is-a?/c bitmap-dc%)
                                exact-nonnegative-integer?
                                exact-nonnegative-integer?
                                hash?
                                void?)]
                                
          ))

(define (locate-timing-pattern-joints modules)
  (let ([joint (+ 9 (- modules 16 1))])
    (list (list (cons 9  7) (cons joint 7))
          (list (cons 7  9) (cons 7 joint)))))

(define (draw-timing-pattern dc modules module_width points_exists_map)
  (let ([joints #f]
        [vertical_joints #f]
        [horizontal_joints #f]
        [horizontal_points #f]
        [vertical_points #f])
  
    (set! joints (locate-timing-pattern-joints modules))
    (set! vertical_joints (car joints))
    (set! horizontal_joints (cadr joints))
    (let ([horizontal_start_point (first horizontal_joints)]
          [horizontal_end_point (second horizontal_joints)]
          [vertical_start_point (first vertical_joints)]
          [vertical_end_point (second vertical_joints)])
      (set! horizontal_points (get-points-between horizontal_start_point horizontal_end_point #:direction 'horizontal))
      (set! vertical_points (get-points-between vertical_start_point vertical_end_point #:direction 'vertical)))

    (let loop ([points vertical_points])
      (when (not (null? points))
            (let ([point (car points)])
              (hash-set! points_exists_map point "timing")
              (if (= (remainder (car point) 2) 1)
                  (draw-module dc "black" (locate-brick module_width point) module_width)
                  (draw-module dc "white" (locate-brick module_width point) module_width)))
            (loop (cdr points))))

    (let loop ([points horizontal_points])
      (when (not (null? points))
            (let ([point (car points)])
              (hash-set! points_exists_map point "timing")
              (if (= (remainder (cdr point) 2) 1)
                  (draw-module dc "black" (locate-brick module_width point) module_width)
                  (draw-module dc "white" (locate-brick module_width point) module_width)))
            (loop (cdr points))))))
