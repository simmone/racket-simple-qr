#lang racket

(require racket/draw)

(require "../share/lib.rkt"
         "../share/qr.rkt"
         "../share/alignment-pattern.rkt")

(provide (contract-out
          [draw-alignment-pattern (-> QR? void?)]
          ))

(define (draw-alignment-pattern qr)
  (for-each
   (lambda (center_point)
     (let ([alignment_points (fill-alignment-pattern-points center_point)])
       (for-each
        (lambda (point)
          (add-point point 1 "alignment" qr))
        (first alignment_points))

       (for-each
        (lambda (point)
          (add-point point 0 "alignment" qr))
        (second alignment_points))

       (for-each
        (lambda (point)
          (add-point point 1 "alignment" qr))
        (third alignment_points))))
     (get-alignment-pattern-center-points version points_map type_map)))
  




