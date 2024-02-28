#lang racket

(require "point.rkt")

;; points_map: point pair => color
;; point pair: '(1 . 1)
;; color: "FF00FF" or "red" or 'transparent
;; one_color: value 1's default color
;; zero_color: value 0's default color
(provide (contract-out
          [struct QR
                  (
                   (mode string?)
                   (error_level string?)
                   (modules natural?)
                   (module_width natural?)
                   (points_map (hash/c POINT? (or/c 1 0)))
                   (type_points_map (hash/c string? (listof POINT?)))
                   (one_color string?)
                   (zero_color (or/c string? 'transparent))
                   )
                  ]
          [add-point (-> POINT? (or/c 1 0) string? QR? void?)]
          ))

(struct QR
        (
         (mode #:mutable)
         (error_level #:mutable)
         (modules #:mutable)
         (module_width #:mutable)
         (points_map #:mutable)
         (type_points_map #:mutable)
         (one_color #:mutable)
         (zero_color #:mutable)
         )
        #:transparent
        )

(define (add-point point val type qr)
  (hash-set! (QR-points_map qr) point value)
  (hash-set! (QR-type_points_map qr) `(,@(hash-ref (QR-type_points_map qr) type '()) ,point)))

