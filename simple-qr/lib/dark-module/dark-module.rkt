#lang racket

(require "../func/func.rkt")

(provide (contract-out
          [draw-dark-module (-> any/c
                              exact-nonnegative-integer?
                              exact-nonnegative-integer?
                              hash?
                              void?)]
          ))

(define (draw-dark-module dc version module_width points_exists_map)
  (let ([point_pair  (cons (+ (* 4 version) 10) 9)])
    (draw-module dc "black" (locate-brick module_width point_pair) module_width)
    (hash-set! points_exists_map point_pair '("1" . "dark"))))
