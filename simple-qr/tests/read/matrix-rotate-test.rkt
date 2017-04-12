#lang racket

(require rackunit/text-ui)

(require rackunit "../../read/matrix-rotate/lib.rkt")

(define test-lib
  (test-suite 
   "test-lib"
   
   (test-case
    "test-matrix->square"

    (let* ([points '((1 2) (3 4))]
           [supplied_points (matrix->square points)])
      (check-equal? supplied_points '((1 2 #f) (3 4 #f) (#f #f #f))))

    (let* ([points '((1 2) (3 4) (5 6))]
           [supplied_points (matrix->square points)])
      (check-equal? supplied_points '((1 2 #f) (3 4 #f) (5 6 #f))))

    (let* ([points '((1 2 3) (3 4 5))]
           [supplied_points (matrix->square points)])
      (check-equal? supplied_points '((1 2 3) (3 4 5) (#f #f #f))))

    (let* ([points '((1 2) (3 4) (5 6) (7 8) (9 10))]
           [supplied_points (matrix->square points)])
      (check-equal? supplied_points '((#f 1 2 #f #f) (#f 3 4 #f #f) (#f 5 6 #f #f) (#f 7 8 #f #f) (#f 9 10 #f #f))))
    )

   (test-case
    "test-matrix->circles"
    
    (let ([points '((1 2 3) (4 5 6) (7 8 9))])
      (check-equal? (matrix->circles points) '((1 2 3 6 9 8 7 4) (5)))
      )

    (let ([points '((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20) (21 22 23 24 25))])
      (check-equal? (matrix->circles points) '(
                                               (1 2 3 4 5 10 15 20 25 24 23 22 21 16 11 6)
                                               (7 8 9 14 19 18 17 12)
                                               (13)))
      )
    )

   (test-case
    "test-circles->matrix"

    (let ([circles '((1 2 3 6 9 8 7 4) (5))])
      (check-equal? (circles->matrix circles)
                    '((1 2 3) (4 5 6) (7 8 9))))

    (let ([circles '(
                     (1 2 3 4 5 10 15 20 25 24 23 22 21 16 11 6)
                     (7 8 9 14 19 18 17 12)
                     (13))])
      (check-equal? (circles->matrix circles) 
                    '((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20) (21 22 23 24 25))))
    )
   
   (test-case
    "test-shift-list"
    
    (let ([origin_list '("a" "b" "c" "d" "e" "f")])
      (check-equal? (shift-list origin_list 1) '("f" "a" "b" "c" "d" "e"))
      (check-equal? (shift-list origin_list -1) '("b" "c" "d" "e" "f" "a"))
      (check-equal? (shift-list origin_list 10) '("c" "d" "e" "f" "a" "b"))
      (check-equal? (shift-list origin_list -10) '("e" "f" "a" "b" "c" "d"))

      (check-equal? (shift-list origin_list 0) '("a" "b" "c" "d" "e" "f"))
      ))

   (test-case
    "test-matrix-rotate"

    (let* ([matrix '((1 2 3) (4 5 6) (7 8 9))]
           [rotated_matrix1 (matrix-rotate matrix (/ 1 8))]
           [rotated_matrix8 (matrix-rotate matrix (/ 8 8))])

      (check-equal? (first (first rotated_matrix1)) 4)

      (check-equal? (first (first rotated_matrix8)) 1)
      )

    (let* ([matrix '((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20) (21 22 23 24 25))]
           [rotated_matrix1 (matrix-rotate matrix (/ 1 16))]
           [rotated_matrix17 (matrix-rotate matrix (/ 17 16))]
           [rotated_matrix_1 (matrix-rotate matrix (/ -1 16))]
           [rotated_matrix_17 (matrix-rotate matrix (/ -17 16))]
           [rotated_matrix16 (matrix-rotate matrix (/ 16 16))]
           [rotated_matrix_16 (matrix-rotate matrix (/ -16 16))])

      (check-equal? matrix rotated_matrix16)
      (check-equal? matrix rotated_matrix_16)

      (check-equal? rotated_matrix1 rotated_matrix17)
      (check-equal? rotated_matrix_1 rotated_matrix_17)
    ))

   (test-case
    "test-matrix-row->col"
    
    (let ([matrix '(
                    (1 2 3 4)
                    (5 6 7 8)
                    (9 10 11 12)
                    )]
          [rotated_matrix '(
                            (9 5 1)
                            (10 6 2)
                            (11 7 3)
                            (12 8 4))])

      (check-equal? (matrix-row->col matrix) rotated_matrix)

      (check-equal? (matrix-col->row rotated_matrix) matrix))
    )
   
   ))

(run-tests test-lib)
