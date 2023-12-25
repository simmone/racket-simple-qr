#lang racket

(require "lib.rkt")
(require "png.rkt")
(require "svg.rkt")

(provide (contract-out
          [draw (-> natural? natural? hash? hash? (cons/c string? string?) path-string? (or/c 'svg 'png) void?)]
          ))

(define (draw modules module_width points_map color_map color file_name output_type)
  (cond
   [(eq? output_type 'svg)
    (draw-svg modules module_width points_map color_map color file_name)]
   [else
    (draw-png modules module_width points_map color_map color file_name)]
   ))

