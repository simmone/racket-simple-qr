#lang racket

(require "lib.rkt")
(require "png.rkt")
(require "svg.rkt")

(provide (contract-out
          [draw (->*
                 (natural? natural? (hash/c (cons/c natural? natural?) string?) path-string? (or/c 'svg 'png))
                 (#:foreground_color string?
                                     #:background_color string?
                                     )
                 void?
                 )
                ]
          ))

;; points_map: point pair => color
;; point pair: '(1 . 1)
;; color: FF00FF or red or transparent
(define (draw modules module_width points_map file_name output_type
              #:foreground_color [foreground_color "black"]
              #:background_color [background_color "white"]
              )
  (cond
   [(eq? output_type 'svg)
    (draw-svg modules module_width points_map foreground_color background_color file_name)]
   [else
    (draw-png modules module_width points_map foreground_color background_color file_name)]
   ))
