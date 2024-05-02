#lang racket

(require "../share/lib.rkt"
         "../share/qr.rkt")

(provide (contract-out
          [draw-alignment-pattern (-> QR? void?)]
          [get-center-point-sets (-> list? list?)]
          [fill-alignment-pattern-points (-> pair? (listof pair?))]
          [get-alignment-pattern-center-points (-> natural? hash? hash? (listof pair?))]
          ))

(define (draw-alignment-pattern qr)
  (for-each
   (lambda (center_point)
     (let ([alignment_points (fill-alignment-pattern-points center_point)])
       (for-each
        (lambda (point)
          (add-point point 1 'alignment qr))
        (first alignment_points))

       (for-each
        (lambda (point)
          (add-point point 0 'alignment qr))
        (second alignment_points))

       (for-each
        (lambda (point)
          (add-point point 1 'alignment qr))
        (third alignment_points))))
     (get-alignment-pattern-center-points (QR-version qr) (QR-point_val_map qr) (QR-point_type_map qr))))
  
(define (get-version-alignment-pattern-list)
  '#hash((1 . ())
         (2 .  (6 18))
         (3 .  (6 22))
         (4 .  (6 26))
         (5 .  (6 30))
         (6 .  (6 34))
         (7 .  (6 22 38))
         (8 .  (6 24 42))
         (9 .  (6 26 46))
         (10 . (6 28 50))
         (11 . (6 30 54))
         (12 . (6 32 58))
         (13 . (6 34 62))
         (14 . (6 26 46 66))
         (15 . (6 26 48 70))
         (16 . (6 26 50 74))
         (17 . (6 30 54 78))
         (18 . (6 30 56 82))
         (19 . (6 30 58 86))
         (20 . (6 34 62 90))
         (21 . (6 28 50 72 94))
         (22 . (6 26 50 74 98))
         (23 . (6 30 54 78 102))
         (24 . (6 28 54 80 106))
         (25 . (6 32 58 84 110))
         (26 . (6 30 58 86 114))
         (27 . (6 34 62 90 118))
         (28 . (6 26 50 74 98 122))
         (29 . (6 30 54 78 102 126))
         (30 . (6 26 52 78 104 130))
         (31 . (6 30 56 82 108 134))
         (32 . (6 34 60 86 112 138))
         (33 . (6 30 58 86 114 142))
         (34 . (6 34 62 90 118 146))
         (35 . (6 30 54 78 102 126 150))
         (36 . (6 24 50 76 102 128 154))
         (37 . (6 28 54 80 106 132 158))
         (38 . (6 32 58 84 110 136 162))
         (39 . (6 26 54 82 110 138 166))
         (40 . (6 30 58 86 114 142 170))
         ))

(define (fill-alignment-pattern-points center_point)
  (let ([x (car center_point)]
        [y (cdr center_point)])
    (list
     (list (cons (- x 2) (- y 2)) (cons (- x 1) (- y 2)) (cons x (- y 2)) (cons (+ x 1) (- y 2)) (cons (+ x 2) (- y 2))
           (cons (- x 2) (- y 1))                                                                (cons (+ x 2) (- y 1))
           (cons (- x 2) y)                                                                      (cons (+ x 2) y)
           (cons (- x 2) (+ y 1))                                                                (cons (+ x 2) (+ y 1))
           (cons (- x 2) (+ y 2)) (cons (- x 1) (+ y 2)) (cons x (+ y 2)) (cons (+ x 1) (+ y 2)) (cons (+ x 2) (+ y 2)))
     (list                        (cons (- x 1) (- y 1)) (cons x (- y 1)) (cons (+ x 1) (- y 1))
                                  (cons (- x 1) y)                        (cons (+ x 1) y)
                                  (cons (- x 1) (+ y 1)) (cons x (+ y 1)) (cons (+ x 1) (+ y 1)))
     (list                                           center_point))))

(define (get-center-point-sets num_list)
  (let loop-out ([out_list num_list]
                 [result_out_list '()])
    (if (not (null? out_list))
        (loop-out (cdr out_list)
                  `(
                    ,@(let loop-in ([in_list num_list]
                                    [result_in_list '()])
                        (if (not (null? in_list))
                            (loop-in
                             (cdr in_list)
                             (cons
                              (cons (car out_list) (car in_list))
                              result_in_list))
                            result_in_list))
                    ,@result_out_list))
        (reverse result_out_list))))

(define (get-alignment-pattern-center-points version point_val_map point_type_map)
  (let loop ([center_points (get-center-point-sets (hash-ref (get-version-alignment-pattern-list) version))]
             [result_list '()])
    (if (not (null? center_points))
        (let* ([center_point (car center_points)]
               [alignment_points 
                (foldr (lambda (a b) (quasiquote ((unquote-splicing a) (unquote-splicing b)))) '() 
                       (fill-alignment-pattern-points (add-quiet-zone-offset center_point)))])

          ;; find if occupied with exists points, exclude "timing pattern" points
          (if (andmap
               (lambda (point)
                 (or
                  (not (hash-has-key? point_val_map point))
                  (eq? (hash-ref point_type_map point) 'timing)))
               alignment_points)
              (loop (cdr center_points) (cons center_point result_list))
              (loop (cdr center_points) result_list)))
        (reverse result_list))))
