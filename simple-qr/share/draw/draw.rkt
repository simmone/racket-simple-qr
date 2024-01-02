#lang racket

(require "lib.rkt")
(require "canvas.rkt")
(require "png.rkt")
(require "svg.rkt")

;; points_map: point pair => color
;; point pair: '(1 . 1)
;; color: FF00FF or red or transparent

(provide (contract-out
          [struct CANVAS
                  (
                   (modules natural?)
                   (module_width natural?)
                   (points_map (hash/c (cons/c natural? natural?) string?))
                   (foreground_color string?)
                   (background_color string?)
                   )
                  ]
          [init-color (-> CANVAS? string? void?)]
          [draw (-> CANVAS? path-string? (or/c 'svg 'png) void?)]
          ))

(define (draw canvas file_name output_type)
  (cond
   [(eq? output_type 'svg)
    (draw-svg canvas file_name)]
   [else
    (draw-png canvas file_name)]
   ))
