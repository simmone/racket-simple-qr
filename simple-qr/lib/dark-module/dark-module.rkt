#lang racket

(require "../func/func.rkt")

(provide (contract-out
          [draw-dark-module (-> exact-nonnegative-integer? hash? void?)]
          ))

(define (draw-dark-module version points_map)
  (let ([point_pair  (cons (+ (* 4 version) 10) 9)])
    (hash-set! points_map point_pair '("1" . "dark"))))
