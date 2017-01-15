#lang racket

(provide (contract-out
         [matrix-rotate (->* ((listof list?) exact-integer?) (#:fill any/c) (listof list?))]
         ))

(require "lib.rkt")

(define (matrix-rotate matrix rotate_number #:fill [fill #f])
  (let* ([square_matrix (matrix->square matrix #:fill fill)]
         [circles (matrix->circles square_matrix)]
         [outtest_circle_length (length (car circles))])
    (circles->matrix
     (let loop ([loop_circles circles]
                [new_circles '()])
       (if (not (null? loop_circles))
           (loop
            (cdr loop_circles)
            (cons
             (shift-list (car loop_circles) (floor (* (/ (length (car loop_circles)) outtest_circle_length) rotate_number)))
             new_circles))
           (reverse new_circles))))))

