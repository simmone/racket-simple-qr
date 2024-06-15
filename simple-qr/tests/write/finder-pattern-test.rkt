#lang racket

(require rackunit/text-ui
         rackunit
         "../../share/qr.rkt"
         "../../write/finder-pattern.rkt")

(define test-finder-pattern
  (test-suite
   "test-finder-pattern"

   (test-case
    "test-locate-finder-pattern"

    (let ([start_points (locate-finder-pattern 21)])
      (check-equal? (first start_points) '(0 . 0))
      (check-equal? (second start_points) '(0 . 14))
      (check-equal? (third start_points) '(14 . 0)))
    )

   (test-case
    "test-draw-finder-pattern"

    (let ([qr (new-default-qr "chenxiao")])
      (fill-finder-pattern qr)

      (check-equal? (hash-count (QR-point_val_map qr)) 147)

      (check-equal? (hash-ref (QR-point_val_map qr) '(4 . 4)) 1)
      (check-equal? (hash-ref (QR-point_val_map qr) '(10 . 10)) 1)
      (check-equal? (hash-ref (QR-point_val_map qr) '(5 . 5)) 0)
      (check-equal? (hash-ref (QR-point_val_map qr) '(9 . 9)) 0)
      (check-equal? (hash-ref (QR-point_val_map qr) '(6 . 6)) 1)
      (check-equal? (hash-ref (QR-point_val_map qr) '(8 . 8)) 1)

      (check-equal? (hash-ref (QR-point_val_map qr) '(4 . 22)) 1)
      (check-equal? (hash-ref (QR-point_val_map qr) '(10 . 28)) 1)
      (check-equal? (hash-ref (QR-point_val_map qr) '(5 . 23)) 0)
      (check-equal? (hash-ref (QR-point_val_map qr) '(9 . 27)) 0)
      (check-equal? (hash-ref (QR-point_val_map qr) '(6 . 24)) 1)
      (check-equal? (hash-ref (QR-point_val_map qr) '(8 . 26)) 1)

      (check-equal? (hash-ref (QR-point_val_map qr) '(22 . 4)) 1)
      (check-equal? (hash-ref (QR-point_val_map qr) '(28 . 10)) 1)
      (check-equal? (hash-ref (QR-point_val_map qr) '(23 . 5)) 0)
      (check-equal? (hash-ref (QR-point_val_map qr) '(27 . 9)) 0)
      (check-equal? (hash-ref (QR-point_val_map qr) '(24 . 6)) 1)
      (check-equal? (hash-ref (QR-point_val_map qr) '(26 . 8)) 1)
    ))
   ))

(run-tests test-finder-pattern)

