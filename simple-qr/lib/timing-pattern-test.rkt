#lang racket

(require rackunit/text-ui)

(require rackunit "timing-pattern.rkt")

(define test-timing-pattern
  (test-suite 
   "test-timing-pattern"

   (test-case 
    "test-locate-timing-pattern-joints"
    
    (let ([joints (locate-timing-pattern-joints 21)])
      (check-equal? (car joints) '((9 . 7) . (13 . 7)))
      (check-equal? (cdr joints) '((7 . 9) . (7 . 13))))

    (let ([joints (locate-timing-pattern-joints 22)])
      (check-equal? (car joints) '((9 . 7) . (14 . 7)))
      (check-equal? (cdr joints) '((7 . 9) . (7 . 14))))

    (let ([joints (locate-timing-pattern-joints 23)])
      (check-equal? (car joints) '((9 . 7) . (15 . 7)))
      (check-equal? (cdr joints) '((7 . 9) . (7 . 15))))

    (let ([joints (locate-timing-pattern-joints 37)])
      (check-equal? (car joints) '((9 . 7) . (29 . 7)))
      (check-equal? (cdr joints) '((7 . 9) . (7 . 29))))
    )
   
   ))

(run-tests test-timing-pattern)
