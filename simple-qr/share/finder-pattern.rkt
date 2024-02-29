#lang racket

(provide (contract-out
          [get-finder-pattern (-> list?)]
          [locate-finder-pattern (-> natural? list?)]
          ))

(define (get-finder-pattern)
  '(
    ((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6) (1 . 7)
     (2 . 1)                                         (2 . 7)
     (3 . 1)                                         (3 . 7)
     (4 . 1)                                         (4 . 7)
     (5 . 1)                                         (5 . 7)
     (6 . 1)                                         (6 . 7)
     (7 . 1) (7 . 2) (7 . 3) (7 . 4) (7 . 5) (7 . 6) (7 . 7))
    (        (2 . 2) (2 . 3) (2 . 4) (2 . 5) (2 . 6)
             (3 . 2)                         (3 . 6)
             (4 . 2)                         (4 . 6)
             (5 . 2)                         (5 . 6)
             (6 . 2) (6 . 3) (6 . 4) (6 . 5) (6 . 6))
    (                (3 . 3) (3 . 4) (3 . 5)
                     (4 . 3) (4 . 4) (4 . 5)
                     (5 . 3) (5 . 4) (5 . 5))))


(define (locate-finder-pattern modules)
  (list
   '(1 . 1)
   (cons 1 (add1 (- modules 7)))
   (cons (add1 (- modules 7)) 1)))
