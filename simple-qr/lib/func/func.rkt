#lang racket

(require racket/draw)

(provide (contract-out
          [version->modules (-> exact-nonnegative-integer? exact-nonnegative-integer?)]
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
          [get-points-between (->
                               pair?
                               pair?
                               #:direction (or/c 'horizontal 'vertical)
                               list?)]
          ))

(define (version->modules version)
  (if (and (>= version 1) (<= version 40))
      (+ 21 (* 4 (sub1 version)))
      (error "invalid version!")))

(define (draw-module dc color place_pair module_width)
  (send dc set-pen color 1 'solid)
  (send dc set-brush color 'solid)

  (send dc draw-rectangle (car place_pair) (cdr place_pair) module_width module_width))

(define (locate-brick module_width place_pair)
  (cons (* (sub1 (cdr place_pair)) module_width)
        (* (sub1 (car place_pair)) module_width)))

(define (locate-finder-pattern modules)
  (list
   '(1 . 1)
   (cons (add1 (- modules 7)) 1)
   (cons 1 (add1 (- modules 7)))))

(define (transform-points-list points_list start_point_pair)
  (map
   (lambda (point)
     (cons (+ (car start_point_pair) (sub1 (car point))) (+ (cdr start_point_pair) (sub1 (cdr point)))))
   points_list))

(define (get-points-between start_point end_point #:direction direction)
  (let ([is_valid? #f])
    (if (equal? direction 'horizontal)
        (when (and
               (= (car start_point) (car end_point))
               (<= (cdr start_point) (cdr end_point)))
              (set! is_valid? #t))
        (when (and
               (= (cdr start_point) (cdr end_point))
               (<= (car start_point) (car end_point)))
              (set! is_valid? #t)))
    (if (not is_valid?)
        '()
        (let ([points '()]
              [vars '()]
              [start_var (if (equal? direction 'horizontal) (cdr start_point) (car start_point))]
              [end_var (if (equal? direction 'horizontal) (cdr end_point) (car end_point))])
          (let loop ([var start_var])
            (when (<= var end_var)
                  (set! vars `(,@vars ,var))
                  (loop (add1 var))))
          (for-each
           (lambda (cor)
             (if (equal? direction 'horizontal)
                 (set! points `(,@points ,(cons (car start_point) cor)))
                 (set! points `(,@points ,(cons cor (cdr start_point))))))
           vars)
          points))))
