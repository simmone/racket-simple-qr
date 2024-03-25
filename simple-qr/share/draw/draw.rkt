#lang racket

(require "../lib.rkt")
(require "../matrix.rkt")

(require "matrix.rkt")
(require "png.rkt")
(require "svg.rkt")

;; points_map: point pair => color
;; point pair: '(1 . 1)
;; color: FF00FF or red or transparent
(provide (contract-out
          [draw (-> MATRIX? path-string? (or/c 'svg 'png 'jpeg 'bmp) void?)]
          ))

(define (draw matrix file_name output_type)
  (cond
   [(eq? output_type 'svg)
    (draw-svg matrix file_name)]
   [else
    (draw-png matrix file_name output_type)]
   ))
