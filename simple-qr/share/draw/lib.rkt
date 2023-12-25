#lang racket

(provide (contract-out
          [locate-brick (-> natural? pair? pair?)]
          ))

(define (locate-brick module_width place_pair)
  (cons (* (sub1 (cdr place_pair)) module_width)
        (* (sub1 (car place_pair)) module_width)))

