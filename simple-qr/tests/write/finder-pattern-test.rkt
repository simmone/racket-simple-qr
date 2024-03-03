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
      
      (check-equal? (hash-ref (QR-points_map qr) '(1 . 1)) 1)

      (check-true (hash-has-key? (QR-type_points_map qr) "finder"))

      (check-equal? (QR-type_points_map qr) (make-hash))
    ))
   ))

(run-tests test-finder-pattern)

