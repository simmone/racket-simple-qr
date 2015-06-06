#lang racket

(require racket/draw)


(provide (contract-out
          [locate-brick (-> exact-nonnegative-integer?
                            pair?
                            pair?)]
          [locate-finder-pattern (->
                                  exact-nonnegative-integer?
                                  list?)]
          [draw-module (-> (is-a?/c bitmap-dc%)
                           (or/c (is-a?/c color%) string?)
                           pair?
                           exact-nonnegative-integer?
                           void?)]
          [transform-points-list (-> list?
                                     pair?
                                     list?)]
          ))

(define (draw-module dc color place_pair module_width)
  (send dc set-pen color 1 'solid)
  (send dc set-brush color 'solid)

  (send dc draw-rectangle (car place_pair) (cdr place_pair) module_width module_width))

(define (locate-brick module_width place_pair)
  (cons (* (sub1 (cdr place_pair)) module_width)
        (* (sub1 (car place_pair)) module_width)))

(define (locate-finder-pattern version)
  (list
   '(1 . 1)
   (cons (add1 (- version 7)) 1)
   (cons 1 (add1 (- version 7)))))

(define (transform-points-list points_list start_point_pair)
  (map
   (lambda (point)
     (cons (+ (car start_point_pair) (sub1 (car point))) (+ (cdr start_point_pair) (sub1 (cdr point)))))
    points_list))
