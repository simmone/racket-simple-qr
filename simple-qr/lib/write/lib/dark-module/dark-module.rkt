#lang racket

(require "../func/func.rkt")
(require "../../../share/dark-module.rkt")

(provide (contract-out
          [draw-dark-module (-> exact-nonnegative-integer? hash? hash? void?)]
          ))

(define (draw-dark-module version points_map type_map)
    (add-point (get-dark-point version) "1" "dark" points_map type_map))
