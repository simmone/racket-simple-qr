#lang racket

(require "../share/qr.rkt")

(provide (contract-out
          [fill-dark-module (-> QR? void?)]
          ))

(define (get-dark-point version)
  (cons (+ (* 4 version) 9) 8))

(define (fill-dark-module qr)
    (add-point (get-dark-point (QR-version qr)) 1 'dark qr))
