#lang racket

(require rackunit/text-ui
         rackunit
         "../../share/qr.rkt"
         "../../write/finder-pattern.rkt")

(require racket/draw)

(define test-finder-pattern
  (test-suite 
   "test-finder-pattern"

   (test-case
    "test-draw-finder-pattern"
    
    (let ([qr (new-default-qr "chenxiao")])
      (draw-finder-pattern qr)
      
      (check-equal? (QR-points_map qr) (make-hash))
      (check-true (hash-has-key? (QR-type_points_map qr) "finder"))
      (check-true (QR-type_points_map qr) (make-hash))
    ))
   ))

(run-tests test-finder-pattern)

