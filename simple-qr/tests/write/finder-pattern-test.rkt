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

      (check-equal? (hash-count (QR-points_map qr)) 147)
      
      (check-equal? (hash-ref (QR-points_map qr) '(1 . 1)) 1)
      (check-equal? (hash-ref (QR-points_map qr) '(7 . 7)) 1)
      (check-equal? (hash-ref (QR-points_map qr) '(2 . 2)) 0)
      (check-equal? (hash-ref (QR-points_map qr) '(6 . 6)) 0)
      (check-equal? (hash-ref (QR-points_map qr) '(3 . 3)) 1)
      (check-equal? (hash-ref (QR-points_map qr) '(5 . 5)) 1)

      (check-equal? (hash-ref (QR-points_map qr) '(1 . 19)) 1)
      (check-equal? (hash-ref (QR-points_map qr) '(7 . 25)) 1)
      (check-equal? (hash-ref (QR-points_map qr) '(2 . 20)) 0)
      (check-equal? (hash-ref (QR-points_map qr) '(6 . 24)) 0)
      (check-equal? (hash-ref (QR-points_map qr) '(3 . 21)) 1)
      (check-equal? (hash-ref (QR-points_map qr) '(5 . 23)) 1)

      (check-equal? (hash-ref (QR-points_map qr) '(19 . 1)) 1)
      (check-equal? (hash-ref (QR-points_map qr) '(25 . 7)) 1)
      (check-equal? (hash-ref (QR-points_map qr) '(20 . 2)) 0)
      (check-equal? (hash-ref (QR-points_map qr) '(24 . 6)) 0)
      (check-equal? (hash-ref (QR-points_map qr) '(21 . 3)) 1)
      (check-equal? (hash-ref (QR-points_map qr) '(23 . 5)) 1)

      (check-true (hash-has-key? (QR-type_points_map qr) "finder"))

      (let ([finder_points (hash-ref (QR-type_points_map qr) "finder")])
        (check-equal? (length finder_points) 147)

        (check-not-false (index-of finder_points '(1 . 1)))
        (check-not-false (index-of finder_points '(7 . 7)))
        (check-not-false (index-of finder_points '(2 . 2)))
        (check-not-false (index-of finder_points '(6 . 6)))
        (check-not-false (index-of finder_points '(3 . 3)))
        (check-not-false (index-of finder_points '(5 . 5)))

        (check-not-false (index-of finder_points '(1 . 19)))
        (check-not-false (index-of finder_points '(7 . 25)))
        (check-not-false (index-of finder_points '(2 . 20)))
        (check-not-false (index-of finder_points '(6 . 24)))
        (check-not-false (index-of finder_points '(3 . 21)))
        (check-not-false (index-of finder_points '(5 . 23)))

        (check-not-false (index-of finder_points '(19 . 1)))
        (check-not-false (index-of finder_points '(25 . 7)))
        (check-not-false (index-of finder_points '(20 . 2)))
        (check-not-false (index-of finder_points '(24 . 6)))
        (check-not-false (index-of finder_points '(21 . 3)))
        (check-not-false (index-of finder_points '(23 . 5)))

        (check-false (index-of finder_points '(7 . 8)))
        (check-false (index-of finder_points '(6 . 18)))
        (check-false (index-of finder_points '(25 . 8)))
        )
    ))
   ))

(run-tests test-finder-pattern)

