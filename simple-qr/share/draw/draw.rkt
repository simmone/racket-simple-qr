#lang racket

(require "lib.rkt")
(require "png.rkt")
(require "svg.rkt")

(provide (contract-out
          [draw (-> natural? natural? (hash/c (cons/c natural? natural?) string?) path-string? (or/c 'svg 'png) void?)]
          ))

;; points_map: point pair => color
;; point pair: '(1 . 1)
;; color: 
(define (draw modules module_width points_map file_name output_type)
  (cond
   [(eq? output_type 'svg)
    (draw-svg modules module_width points_map file_name)]
   [else
    (draw-png modules module_width points_map file_name)]
   ))

