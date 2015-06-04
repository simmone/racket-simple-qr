#lang racket

(require racket/draw)

(provide (contract-out
          [locate-brick (-> exact-nonnegative-integer?
                            pair?
                            pair?)]
          [white-block (-> any/c
                           pair?
                           pair?
                           void?)]
          [black-block (-> any/c
                           pair?
                           pair?
                           void?)]
          [locate-finder-pattern (->
                                  exact-nonnegative-integer?
                                  exact-nonnegative-integer?                                  
                                  (values
                                   pair?
                                   pair?
                                   pair?))]
          ))

(define (white-block dc place_pair size_pair)
  (send dc set-pen "white" 1 'solid)
  (send dc set-brush "white" 'solid)

  (send dc draw-rectangle (car place_pair) (cdr place_pair) (car size_pair) (cdr size_pair)))

(define (black-block dc place_pair size_pair)
  (send dc set-pen "black" 1 'solid)
  (send dc set-brush "black" 'solid)

  (send dc draw-rectangle (car place_pair) (cdr place_pair) (car size_pair) (cdr size_pair)))

(define (locate-brick brick_width place_pair)
  (cons (+ brick_width (* (sub1 (cdr place_pair)) brick_width))
        (+ brick_width (* (sub1 (car place_pair)) brick_width))))

(define (locate-finder-pattern version module_width)
  (values
   (cons module_width module_width)
   (cons (- (* version module_width) module_width (* 7 module_width)) module_width)
   (cons module_width (- (* version module_width) module_width (* 7 module_width)))))


(define (draw-finder-patter dc version module_width)
  
