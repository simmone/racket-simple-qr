#lang racket

(provide (contract-out
          [get-version-points (-> (listof list?))]
          ))

(define (get-version-points)
  '(
    ((1 . 1) (1 . 2) (1 . 3)
     (2 . 1) (2 . 2) (2 . 3)
     (3 . 1) (3 . 2) (3 . 3)
     (4 . 1) (4 . 2) (4 . 3)
     (5 . 1) (5 . 2) (5 . 3)
     (6 . 1) (6 . 2) (6 . 3))
    ((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6)
     (2 . 1) (2 . 2) (2 . 3) (2 . 4) (2 . 5) (2 . 6)
     (3 . 1) (3 . 2) (3 . 3) (3 . 4) (3 . 5) (3 . 6))
    ))