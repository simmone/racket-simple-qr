#lang racket

(require "../lib.rkt")

(provide (contract-out
          [struct MATRIX
                  (
                   (width natural?)
                   (bricks natural?)
                   (brick_width natural?)
                   (points (listof (cons/c natural? natural?)))
                   (points_color_map (hash/c (cons/c natural? natural?) (or/c string? 'transparent)))
                  )]
          [new-matrix (-> natural? MATRIX?)]
          [fill-points (-> MATRIX? (listof (cons/c natural? natural?)) string? void?)]
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

(define (fill-points matrix points color)
  (for-each
   (lambda (point)
     (hash-set! (MATRIX-points_color_map matrix) point color))
   points))

