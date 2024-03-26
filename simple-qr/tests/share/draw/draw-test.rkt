#lang racket

(require rackunit/text-ui
         rackunit
         "../../../share/draw/matrix.rkt"
         "../../../share/lib.rkt"
         "../../../share/qr.rkt"
         "../../../share/draw/draw.rkt"
         "../../../share/finder-pattern.rkt"
         racket/runtime-path)

(define-runtime-path png_1X1_file "png_1X1.png")
(define-runtime-path svg_1X1_file "svg_1X1.svg")
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
    "test-draw-1X1"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([matrix (new-matrix 1)])
            (fill-points matrix '((0 . 0)) "black")
;            (draw matrix png_1X1_file 'png)
            (draw matrix svg_1X1_file 'svg)
            ))
        (lambda ()
          (void)
          ;(delete-file canvas_png_file)
          ;(delete-file canvas_svg_file)
          )))
;
;   (test-case
;    "test-init-canvas-with-quiet-zone"
;    
;    (dynamic-wind
;        (lambda () (void))
;        (lambda ()
;          (let ([qr (new-default-qr "")])
;            (draw qr canvas_png_file 'png)
;            (draw qr canvas_svg_file 'svg)
;            ))
;        (lambda ()
;          (void)
;          ;(delete-file canvas_png_file)
;          ;(delete-file canvas_svg_file)
;          )))
;
;   (test-case
;    "test-fill-canvas"
;    
;    (dynamic-wind
;        (lambda () (void))
;        (lambda ()
;          (let ([qr (new-default-qr "chenxiao")])
;            (set-QR-module_width! qr 20)
;
;            (draw qr fill_canvas_png_file 'png)
;            (draw qr fill_canvas_svg_file 'svg)
;            ))
;        (lambda ()
;          ;(void)
;          (delete-file fill_canvas_png_file)
;          (delete-file fill_canvas_svg_file)
;          )))
;
;   (test-case
;    "test-pattern-canvas"
;    
;    (dynamic-wind
;        (lambda () (void))
;        (lambda ()
;          (let ([qr (new-default-qr "chenxiao")])
;            (set-QR-module_width! qr 20)
;
;            (draw qr pattern_canvas_png_file 'png)
;            (draw qr pattern_canvas_svg_file 'svg)
;            ))
;        (lambda ()
;          ;(void)
;          (delete-file pattern_canvas_png_file)
;          (delete-file pattern_canvas_svg_file)
;          )))
;   
;   (test-case
;    "test-transparent-canvas"
;
;    (dynamic-wind
;        (lambda () (void))
;        (lambda ()
;          (let ([qr (new-default-qr "chenxiao")])
;            (set-QR-module_width! qr 20)
;
;            (draw qr transparent_canvas_png_file 'png)
;            (draw qr transparent_canvas_svg_file 'svg)
;            ))
;        (lambda ()
;          ;(void)
;          (delete-file transparent_canvas_png_file)
;          (delete-file transparent_canvas_svg_file)
;          )))
;
;   (test-case
;    "test-fill-points"
;    
;    (dynamic-wind
;        (lambda () (void))
;        (lambda ()
;          (let ([qr (new-default-qr "chenxiao")])
;            (set-QR-module_width! qr 20)
;
;            (draw qr normal_canvas_png_file 'png)
;            (draw qr normal_canvas_svg_file 'svg)
;            ))
;        (lambda ()
;          ;(void)
;          (delete-file normal_canvas_png_file)
;          (delete-file normal_canvas_svg_file)
;          )))
   ))

(run-tests test-func)
