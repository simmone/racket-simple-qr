#lang racket

(require "point.rkt")

;; points_map: point pair => color
;; point pair: '(1 . 1)
;; color: "FF00FF" or "red" or 'transparent
;; one_color: value 1's default color
;; zero_color: value 0's default color
(provide (contract-out
          [struct CANVAS
                  (
                   (modules natural?)
                   (module_width natural?)
                   (points_map (hash/c POINT? (or/c 1 0)))
                   (type_points_map (hash/c string? (listof POINT?)))
                   (one_color string?)
                   (zero_color (or/c string? 'transparent))
                   )
                  ]
          [add-point (-> POINT? (or/c 1 0) string? CANVAS? void?)]
          ))

(struct CANVAS
        (
         (modules #:mutable)
         (module_width #:mutable)
         (points_map #:mutable)
         (type_points_map #:mutable)
         (one_color #:mutable)
         (zero_color #:mutable)
         )
        #:transparent
        )

(define (add-point point val type canvas)
  (hash-set! (CANVAS-points_map canvas) point value)
  (hash-set! (CANVAS-type_points_map canvas) `(,@(hash-ref (CANVAS-type_points_map canvas) type '()) ,point)))

