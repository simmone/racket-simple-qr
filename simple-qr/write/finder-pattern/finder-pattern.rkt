#lang racket

(require "../../share/qr.rkt")
(require "../../share/func.rkt")
(require "../../share/finder-pattern.rkt")

(provide (contract-out
          [draw-finder-pattern (-> QR? void?)]
          ))

(define (draw-finder-pattern qr)
  (for-each
   (lambda (start_point)
     (for-each
      (lambda (point_pair)
        (add-point point_pair 1 "finder" qr))
      (transform-points-list (first (get-finder-pattern)) start_point))

     (for-each
      (lambda (point_pair)
        (add-point point_pair 0 "finder" qr))
      (transform-points-list (second (get-finder-pattern)) start_point))

     (for-each
      (lambda (point_pair)
        (add-point point_pair 1 "finder" qr))
      (transform-points-list (third (get-finder-pattern)) start_point)))
   (locate-finder-pattern modules)))
