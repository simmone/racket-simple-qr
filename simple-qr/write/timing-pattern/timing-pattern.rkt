#lang racket

(require racket/draw
         "../share/lib.rkt"
         "../../../share/timing-pattern.rkt"
         "../../../share/qr.rkt")

(provide (contract-out
          [draw-timing-pattern (-> QR? void?)]
          ))

(define (draw-timing-pattern qr)
  (let ([timing_pattern_points (get-timing-pattern-points modules)])

    (let loop ([points (first timing_pattern_points)])
      (when (not (null? points))
            (let ([point (car points)])
              (if (= (remainder (cdr point) 2) 1)
                  (add-point point 1 "timing" points_map type_map)
                  (add-point point 0 "timing" points_map type_map)))
            (loop (cdr points))))

    (let loop ([points (second timing_pattern_points)])
      (when (not (null? points))
            (let ([point (car points)])
              (if (= (remainder (car point) 2) 1)
                  (add-point point 1 "timing" points_map type_map)
                  (add-point point 0 "timing" points_map type_map)))
            (loop (cdr points))))))
