#lang racket

(require "lib.rkt")
(require "png.rkt")
(require "svg.rkt")

(provide (contract-out
          [draw (-> natural? natural? hash? hash? path-string? void?)]
          [*output_type* parameter?]
          ))

(define (draw modules module_width points_map color_map file_name)
  (cond
   [(eq? (*output_type*) 'svg)
    (draw-svg modules module_width points_map color_map file_name)]
   [else
    (draw-png modules module_width points_map color_map file_name)]
   ))

