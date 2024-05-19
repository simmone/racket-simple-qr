#lang racket

(require "../share/lib.rkt"
         "finder-pattern.rkt"
         "../share/qr.rkt")

(provide (contract-out
          [fill-separator (-> QR? void?)]
          [get-separator (-> list?)]
          ))

(define (fill-separator qr)
  (let* ([finder_pattern_start_points (locate-finder-pattern (QR-modules qr))]
         [top_left_point (first finder_pattern_start_points)]
         [top_right_point (second finder_pattern_start_points)]
         [bottom_left_point (third finder_pattern_start_points)]
         [new_top_right_point (cons (car top_right_point) (sub1 (cdr top_right_point)))]
         [new_bottom_left_point (cons (sub1 (car bottom_left_point)) (cdr bottom_left_point))])

    (for-each
     (lambda (point)
       (add-point point 0 'separator qr))
     (transform-points-list (first (get-separator)) top_left_point))

     (for-each
      (lambda (point)
        (add-point point 0 'separator qr))
      (transform-points-list (second (get-separator)) new_top_right_point))

     (for-each
      (lambda (point)
        (add-point point 0 'separator qr))
      (transform-points-list (third (get-separator)) new_bottom_left_point))
     ))

(define (get-separator)
  '(
    (                                                        (0 . 7)
                                                             (1 . 7)
                                                             (2 . 7)
                                                             (3 . 7)
                                                             (4 . 7)
                                                             (5 . 7)
                                                             (6 . 7)
     (7 . 0) (7 . 1) (7 . 2) (7 . 3) (7 . 4) (7 . 5) (7 . 6) (7 . 7))
    ((0 . 0)
     (1 . 0)
     (2 . 0)
     (3 . 0)
     (4 . 0)
     (5 . 0)
     (6 . 0)
     (7 . 0) (7 . 1) (7 . 2) (7 . 3) (7 . 4) (7 . 5) (7 . 6) (7 . 7))
    ((0 . 0) (0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . 5) (0 . 6) (0 . 7)
                                                             (1 . 7)
                                                             (2 . 7)
                                                             (3 . 7)
                                                             (4 . 7)
                                                             (5 . 7)
                                                             (6 . 7)
                                                             (7 . 7))
    ))
