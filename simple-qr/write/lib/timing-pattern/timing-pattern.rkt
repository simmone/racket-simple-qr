#lang racket

(require racket/draw)

(require "../func/func.rkt")
(require "../../../share/timing-pattern.rkt")
(require "../../../share/func.rkt")

(provide (contract-out
          [draw-timing-pattern (-> exact-nonnegative-integer? hash? hash? void?)]
          ))

(define (draw-timing-pattern modules points_map type_map)
  (let ([timing_pattern_points (get-timing-pattern-points modules)])

    (let loop ([points (first timing_pattern_points)])
      (when (not (null? points))
            (let ([point (car points)])
              (if (= (remainder (cdr point) 2) 1)
                  (add-point point "1" "timing" points_map type_map)
                  (add-point point "0" "timing" points_map type_map)))
            (loop (cdr points))))

    (let loop ([points (second timing_pattern_points)])
      (when (not (null? points))
            (let ([point (car points)])
              (if (= (remainder (car point) 2) 1)
                  (add-point point "1" "timing" points_map type_map)
                  (add-point point "0" "timing" points_map type_map)))
            (loop (cdr points))))))
