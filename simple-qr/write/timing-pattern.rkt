#lang racket

(require racket/draw
         "../share/lib.rkt"
         "../share/timing-pattern.rkt"
         "../share/qr.rkt")

(provide (contract-out
          [draw-timing-pattern (-> QR? void?)]
          ))

(define (draw-timing-pattern qr)
  (let ([timing_pattern_points (get-timing-pattern-points (QR-modules qr))])

    (let loop ([points (first timing_pattern_points)]
               [index 0])
      (when (not (null? points))
            (let ([point (car points)])
              (if (even? index)
                  (add-point point 1 'timing qr)
                  (add-point point 0 'timing qr)))
            (loop (cdr points) (add1 index))))

    (let loop ([points (second timing_pattern_points)]
               [index 0])
      (when (not (null? points))
            (let ([point (car points)])
              (if (even? index)
                  (add-point point 1 'timing qr)
                  (add-point point 0 'timing qr)))
            (loop (cdr points) (add1 index))))))
