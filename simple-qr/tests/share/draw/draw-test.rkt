#lang racket

(require rackunit/text-ui)

(require rackunit)
(require "../../../share/draw/lib.rkt")
(require "../../../share/draw/draw.rkt")
(require "../../../share/finder-pattern.rkt")

(require racket/runtime-path)
(define-runtime-path canvas_file "canvas.png")
(define-runtime-path pattern_canvas_file "pattern_canvas.png")
(define-runtime-path transparent_canvas_file "transparent_canvas.png")
(define-runtime-path normal_canvas_file "normal_canvas.png")

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
    "test-canvas"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([canvas (CANVAS 20 10 (make-hash) "black" "white")])
            (draw canvas canvas_file 'png)))
        (lambda ()
          ;;(void)
          (delete-file canvas_file)
          )))

   (test-case
    "test-pattern-canvas"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([canvas (CANVAS 5 20 (make-hash) "black" "white")])
            (init-color canvas "pattern")
            (draw canvas pattern_canvas_file 'png)))
        (lambda ()
          (void)
          ;;(delete-file pattern_canvas_file)
          )))
   
   (test-case
    "test-transparent-canvas"

    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([canvas (CANVAS 20 10 (make-hash) "black" "transparent")])
            (draw canvas transparent_canvas_file 'png)))
        (lambda ()
          ;;(void)
          (delete-file transparent_canvas_file)
          )))

   (test-case
    "test-fill-points"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([canvas (CANVAS 20 10 (make-hash) "black" "white")])
            (for-each
             (lambda (point_pair)
               (hash-set! (CANVAS-points_map canvas) point_pair "red"))
             (first (get-finder-pattern)))

            (for-each
             (lambda (point_pair)
               (hash-set! (CANVAS-points_map canvas) point_pair "red"))
             (third (get-finder-pattern)))

            (draw canvas normal_canvas_file 'png)))
        (lambda ()
          ;;(void)
          (delete-file normal_canvas_file)
          )))

   ))

(run-tests test-func)
