#lang racket

(require "../../share/qr.rkt")
(require "../../share/finder-pattern.rkt")
(require "../../share/func.rkt")

(provide (contract-out
          [draw-finder-pattern (-> QR? void?)]
          ))

(define (draw-finder-pattern qr)
  (for-each
   (lambda (start_point)
     (for-each
      (lambda (point)
        (add-point point 1 "finder" qr))
      (transform-points-list (first (get-finder-pattern)) start_point))

     (for-each
      (lambda (point)
        (add-point point 0 "finder" qr))
      (transform-points-list (second (get-finder-pattern)) start_point))

     (for-each
      (lambda (point)
        (add-point point 1 "finder" qr))
      (transform-points-list (third (get-finder-pattern)) start_point)))
   (locate-finder-pattern (QR-modules qr))))
