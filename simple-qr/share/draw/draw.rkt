#lang racket

(require "../lib.rkt"
         "matrix.rkt"
         "png.rkt"
         "svg.rkt")

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
