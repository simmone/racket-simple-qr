#lang racket

(require racket/draw)

(require "../func/func.rkt")

(provide (contract-out
          [get-alignment-pattern-points (->
                                         pair?
                                         list?)]
          [get-center-point-sets (->
                                  list?
                                  list?)]
          [draw-alignment-pattern (-> any/c
                                      exact-nonnegative-integer?
                                      exact-nonnegative-integer?
                                      hash?
                                      void?)]
          ))

(define *alignment_pattern_map*
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

(define (get-alignment-pattern-points center_point)
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
  (reverse
   (let loop1 ([loop1_list num_list]
               [result1_list '()])
     (if (not (null? loop1_list))
         (loop1 (cdr loop1_list) (quasiquote
                                  (
                                   (unquote-splicing
                                    (let loop2 ([loop2_list num_list]
                                                [result2_list '()])
                                      (if (not (null? loop2_list))
                                          (loop2 (cdr loop2_list) (cons (cons (car loop1_list) (car loop2_list)) result2_list))
                                          result2_list)))
                                   (unquote-splicing result1_list))))
         result1_list))))

(define (draw-alignment-pattern dc version module_width points_exists_map)
  (for-each
   (lambda (center_point_origin)
     (let ([center_point (cons (add1 (car center_point_origin)) (add1 (cdr center_point_origin)))])
       (let ([alignment_points (get-alignment-pattern-points center_point)])
         ;; find if occupied with exists points
         (when (andmap
                (lambda (point)
                  (or (not (hash-has-key? points_exists_map point))
                      (and (hash-has-key? points_exists_map point) (string=? (hash-ref points_exists_map point) "timing"))))
                (foldr (lambda (a b) (quasiquote ((unquote-splicing a) (unquote-splicing b)))) '() alignment_points))
               (for-each
                (lambda (point_pair)
                  (draw-module dc "black" (locate-brick module_width point_pair) module_width)
                  (hash-set! points_exists_map point_pair "alignment"))
                (first alignment_points))

               (for-each
                (lambda (point_pair)
                  (draw-module dc "white" (locate-brick module_width point_pair) module_width)
                  (hash-set! points_exists_map point_pair "alignment"))
                (second alignment_points))

               (for-each
                (lambda (point_pair)
                  (draw-module dc "black" (locate-brick module_width point_pair) module_width)
                  (hash-set! points_exists_map point_pair "alignment"))
                (third alignment_points))))))
     (get-center-point-sets (hash-ref *alignment_pattern_map* version))))

  




