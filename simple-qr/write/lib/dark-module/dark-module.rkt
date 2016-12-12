#lang racket

(require "../func/func.rkt")

(provide (contract-out
          [draw-dark-module (-> exact-nonnegative-integer? hash? hash? void?)]
          ))

(define (draw-dark-module version points_map type_map)
  (let ([point_pair  (cons (+ (* 4 version) 10) 9)])
    (add-point point_pair "1" "dark" points_map type_map)))
