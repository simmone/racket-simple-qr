#lang racket

(require rackunit/text-ui)

(require rackunit)
(require "../../../share/draw/lib.rkt")
(require "../../../share/draw/draw.rkt")
(require "../../../share/finder-pattern.rkt")

(require racket/runtime-path)
(define-runtime-path canvas_file "canvas.png")

(define test-func
  (test-suite 
   "test-func"

   (test-case 
    "test-locate-brick"
    
    (let ([place_pair (locate-brick 1 (cons 1 1))])
      (check-equal? place_pair '(0 . 0)))

    (let ([place_pair (locate-brick 2 (cons 1 1))])
      (check-equal? place_pair '(0 . 0)))

    (let ([place_pair (locate-brick 2 (cons 2 2))])
      (check-equal? place_pair '(2 . 2)))

    (let ([place_pair (locate-brick 2 (cons 5 5))])
      (check-equal? place_pair '(8 . 8)))

    (let ([place_pair (locate-brick 3 (cons 5 5))])
      (check-equal? place_pair '(12 . 12)))

    (let ([place_pair (locate-brick 3 (cons 5 7))])
      (check-equal? place_pair '(18 . 12)))
    )
   
   (test-case
    "test-canvas-generation"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([canvas (CANVAS 10 5 (make-hash) "black" "white")])
            (draw canvas canvas_file 'png)))
        (lambda ()
          (void)
          ;;(delete-file png_normal_file)
          )))
   ))

(run-tests test-func)
