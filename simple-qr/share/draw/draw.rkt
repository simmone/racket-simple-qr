#lang racket

(require "../lib.rkt")
(require "../qr.rkt")

(require "png.rkt")
(require "svg.rkt")

;; points_map: point pair => color
;; point pair: '(1 . 1)
;; color: FF00FF or red or transparent
(provide (contract-out
          [draw (-> QR? path-string? (or/c 'svg 'png 'jpeg 'bmp) void?)]
          ))

(define (draw qr file_name output_type)
  (cond
   [(eq? output_type 'svg)
    (draw-svg qr file_name)]
   [else
    (draw-png qr file_name output_type)]
   ))
