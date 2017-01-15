#lang racket

(require rackunit/text-ui)

(require rackunit "main.rkt")

(require "lib.rkt")

(define test-main
  (test-suite
   "test-main"

   (test-case
    "test-matrix-rotate"

    (let* ([matrix '((1 2 3) (4 5 6) (7 8 9))]
           [rotated_matrix1 (matrix-rotate matrix 1)]
           [rotated_matrix8 (matrix-rotate matrix 8)])

      (check-equal? (first (first rotated_matrix1)) 4)

      (check-equal? (first (first rotated_matrix8)) 1)
      )

    (let* ([matrix '((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20) (21 22 23 24 25))]
           [rotated_matrix1 (matrix-rotate matrix 1)]
           [rotated_matrix17 (matrix-rotate matrix 17)]
           [rotated_matrix_1 (matrix-rotate matrix -1)]
           [rotated_matrix_17 (matrix-rotate matrix -17)]
           [rotated_matrix16 (matrix-rotate matrix 16)]
           [rotated_matrix_16 (matrix-rotate matrix -16)])

      (check-equal? matrix rotated_matrix16)
      (check-equal? matrix rotated_matrix_16)

      (check-equal? rotated_matrix1 rotated_matrix17)
      (check-equal? rotated_matrix_1 rotated_matrix_17)
    ))

   ))

(run-tests test-main)
