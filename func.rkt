#lang racket

(require racket/draw)

(provide (contract-out
          [locate-brick (-> #:matrix exact-nonnegative-integer?
                            #:brick_width exact-nonnegative-integer?
                            #:row exact-nonnegative-integer?
                            #:col exact-nonnegative-integer?
                            (values exact-nonnegative-integer? exact-nonnegative-integer?))]
          [white-block (-> #:dc (is-a?/c bitmap%)
                           #:x exact-nonnegative-integer?
                           #:y exact-nonnegative-integer?
                           #:width exact-nonnegative-integer?
                           #:height exact-nonnegative-integer?
                           void?)]
          [black-block (-> #:dc (is-a?/c bitmap%)
                           #:x exact-nonnegative-integer?
                           #:y exact-nonnegative-integer?
                           #:width exact-nonnegative-integer?
                           #:height exact-nonnegative-integer?
                           void?)]
          ))

(define (white-block #:dc dc #:x x #:y y #:width width #:height height)
  (send dc set-pen "white" 1 'solid)
  (send dc set-block "white" 'solid)

  (send dc draw-rectangle x y width height))

(define (black-block #:dc dc #:x x #:y y #:width width #:height height)
  (send dc set-pen "black" 1 'solid)
  (send dc set-block "black" 'solid)

  (send dc draw-rectangle x y width height))

(define (locate-brick #:matrix matrix #:brick_width brick_width #:row row #:col col)
  (values 1 1)
  )
