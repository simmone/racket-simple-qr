#lang racket

(require "../lib.rkt")

(provide (contract-out
          [struct MATRIX
                  (
                   (width natural?)
                   (bricks natural?)
                   (brick_width natural?)
                   (points (listof (cons/c natural? natural?)))
                   (points_color_map (hash/c (cons/c natural? natural?) (or/c string? 'pattern1)))
                  )]
          [new-matrix (-> natural? natural? MATRIX?)]
          [fill-point-color (-> MATRIX? (cons/c natural? natural?) (or/c string? 'pattern1 'transparent) void?)]
          [fill-points-color (-> MATRIX? (listof (cons/c natural? natural?)) (listof (or/c string? 'pattern1 'transparent)) void?)]
          ))

(struct MATRIX
        (
         (width #:mutable)
         (bricks #:mutable)
         (brick_width #:mutable)
         (points #:mutable)
         (points_color_map #:mutable)
         )
        #:transparent
        )

(define (new-matrix bricks brick_width)
  (let* ([width (* bricks brick_width)]
         [points (get-points-between '(0 . 0) (cons (sub1 bricks) (sub1 bricks)) #:direction 'cross)])
    (MATRIX width bricks brick_width points (make-hash))))

(define (fill-point-color matrix point color)
  (when (not (eq? color 'transparent))
    (hash-set! (MATRIX-points_color_map matrix) point color)))

(define (fill-points-color matrix points colors)
  (let loop-point ([loop_points points]
                   [index 0])
    (when (not (null? loop_points))
      (fill-point-color matrix
                        (car loop_points) (list-ref colors (remainder index (length colors))))
      (loop-point (cdr loop_points) (add1 index)))))
