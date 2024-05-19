#lang racket

(require "../share/lib.rkt"
         "../share/qr.rkt")

(provide (contract-out
          [fill-finder-pattern (-> QR? void?)]
          [locate-finder-pattern (-> natural? list?)]
          ))

(define (fill-finder-pattern qr)
  (for-each
   (lambda (start_point)
     (for-each
      (lambda (point)
        (add-point point 1 'finder qr))
      (transform-points-list (first (get-finder-pattern)) start_point))

     (for-each
      (lambda (point)
        (add-point point 0 'finder qr))
      (transform-points-list (second (get-finder-pattern)) start_point))

     (for-each
      (lambda (point)
        (add-point point 1 'finder qr))
      (transform-points-list (third (get-finder-pattern)) start_point)))
   (locate-finder-pattern (QR-modules qr))))

(define (get-finder-pattern)
  '(
    ((0 . 0) (0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . 5) (0 . 6)
     (1 . 0)                                         (1 . 6)
     (2 . 0)                                         (2 . 6)
     (3 . 0)                                         (3 . 6)
     (4 . 0)                                         (4 . 6)
     (5 . 0)                                         (5 . 6)
     (6 . 0) (6 . 1) (6 . 2) (6 . 3) (6 . 4) (6 . 5) (6 . 6))

    (        (1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5)
             (2 . 1)                         (2 . 5)
             (3 . 1)                         (3 . 5)
             (4 . 1)                         (4 . 5)
             (5 . 1) (5 . 2) (5 . 3) (5 . 4) (5 . 5))

    (                (2 . 2) (2 . 3) (2 . 4)
                     (3 . 2) (3 . 3) (3 . 4)
                     (4 . 2) (4 . 3) (4 . 4))
    ))

(define (locate-finder-pattern modules)
  (list
   '(0 . 0)
   (cons 0 (- modules 7))
   (cons (- modules 7) 0)))
