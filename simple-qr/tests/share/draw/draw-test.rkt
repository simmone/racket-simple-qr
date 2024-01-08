#lang racket

(require rackunit/text-ui)

(require rackunit)
(require "../../../share/draw/lib.rkt")
(require "../../../share/draw/draw.rkt")
(require "../../../share/finder-pattern.rkt")

(require racket/runtime-path)
(define-runtime-path canvas_png_file "canvas.png")
(define-runtime-path canvas_svg_file "canvas.svg")
(define-runtime-path fill_canvas_png_file "fill_canvas.png")
(define-runtime-path fill_canvas_svg_file "fill_canvas.svg")
(define-runtime-path pattern_canvas_png_file "pattern_canvas.png")
(define-runtime-path pattern_canvas_svg_file "pattern_canvas.svg")
(define-runtime-path transparent_canvas_png_file "transparent_canvas.png")
(define-runtime-path transparent_canvas_svg_file "transparent_canvas.svg")
(define-runtime-path normal_canvas_png_file "normal_canvas.png")
(define-runtime-path normal_canvas_svg_file "normal_canvas.svg")
(define-runtime-path normal_canvas_jpg_file "normal_canvas.jpg")
(define-runtime-path normal_canvas_bmp_file "normal_canvas.bmp")

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
            (draw canvas canvas_png_file 'png)
            (draw canvas canvas_svg_file 'svg)
            ))
        (lambda ()
          ;(void)
          (delete-file canvas_png_file)
          (delete-file canvas_svg_file)
          )))

   (test-case
    "test-fill-canvas"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([canvas (CANVAS 20 10 (make-hash) "black" "white")])
            (init-color canvas "red")
            (draw canvas fill_canvas_png_file 'png)
            (draw canvas fill_canvas_svg_file 'svg)
            ))
        (lambda ()
          ;(void)
          (delete-file fill_canvas_png_file)
          (delete-file fill_canvas_svg_file)
          )))

   (test-case
    "test-pattern-canvas"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([canvas (CANVAS 5 20 (make-hash) "black" "white")])
            (init-color canvas "pattern")
            (draw canvas pattern_canvas_png_file 'png)
            (draw canvas pattern_canvas_svg_file 'svg)
            ))
        (lambda ()
          ;(void)
          (delete-file pattern_canvas_png_file)
          ;(delete-file pattern_canvas_svg_file)
          )))
   
   (test-case
    "test-transparent-canvas"

    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([canvas (CANVAS 20 10 (make-hash) "black" "transparent")])
            (draw canvas transparent_canvas_png_file 'png)
            (draw canvas transparent_canvas_svg_file 'svg)
            ))
        (lambda ()
          ;(void)
          (delete-file transparent_canvas_png_file)
          (delete-file transparent_canvas_svg_file)
          )))

   (test-case
    "test-fill-points"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([canvas (CANVAS 20 10 (make-hash) "black" "white")])

            (init-color canvas "pattern")
            
            (fill-color canvas (first (get-finder-pattern)) "red")
            (fill-color canvas (second (get-finder-pattern)) "white")
            (fill-color canvas (third (get-finder-pattern)) "red")

            (draw canvas normal_canvas_png_file 'png)
            (draw canvas normal_canvas_svg_file 'svg)
            ;(draw canvas normal_canvas_jpg_file 'jpeg)
            ;(draw canvas normal_canvas_bmp_file 'bmp)
            ))
        (lambda ()
          (void)
          ;(delete-file normal_canvas_png_file)
          ;(delete-file normal_canvas_svg_file)
          ;(delete-file normal_canvas_jpg_file)
          ;(delete-file normal_canvas_bmp_file)
          )))
   ))

(run-tests test-func)
