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
(define-runtime-path png_2X2_file "png_2X2.png")
(define-runtime-path svg_2X2_file "svg_2X2.svg")
(define-runtime-path png_3X3_file "png_3X3.png")
(define-runtime-path svg_3X3_file "svg_3X3.svg")
(define-runtime-path svg_1X1_pattern1_file "svg_1X1_pattern1.svg")
(define-runtime-path png_5X5_file "png_5X5.png")
(define-runtime-path svg_5X5_file "svg_5X5.svg")

(define test-func
  (test-suite 
   "test-func"

   (test-case
    "test-draw-1X1"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([matrix (new-matrix 1 50)])
            (fill-points matrix '((0 . 0)) "red")
            (draw matrix png_1X1_file 'png)
            (draw matrix svg_1X1_file 'svg)
            ))
        (lambda ()
          ;(void)
          (delete-file svg_1X1_file)
          (delete-file png_1X1_file)
          )))

   (test-case
    "test-draw-2X2"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([matrix (new-matrix 2 50)])
            (fill-points matrix '((0 . 0) (1 . 1)) "red")
            (fill-points matrix '((0 . 1) (1 . 0)) "yellow")
            (draw matrix png_2X2_file 'png)
            (draw matrix svg_2X2_file 'svg)
            ))
        (lambda ()
          ;(void)
          (delete-file svg_2X2_file)
          (delete-file png_2X2_file)
          )))

   (test-case
    "test-draw-3X3"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([matrix (new-matrix 3 100)])
            (let loop ([points (MATRIX-points matrix)]
                       [index 1])
              (when (not (null? points))
                (if (odd? index)
                    (fill-points matrix (list (car points)) "red")
                    (fill-points matrix (list (car points)) "yellow"))
                (loop (cdr points) (add1 index))))
            (draw matrix png_3X3_file 'png)
            (draw matrix svg_3X3_file 'svg)
            ))
        (lambda ()
          ;(void)
          (delete-file svg_3X3_file)
          (delete-file png_3X3_file)
          )))

   (test-case
    "test-draw-1X1-pattern1"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([matrix (new-matrix 1 60)])
            (fill-points matrix '((0 . 0)) 'pattern1)
            (draw matrix svg_1X1_pattern1_file 'svg)
            ))
        (lambda ()
          ;(void)
          (delete-file svg_1X1_pattern1_file)
          )))

   (test-case
    "test-draw-5X5"
    
    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([matrix (new-matrix 5 100)])
            (let loop ([points (MATRIX-points matrix)]
                       [index 1])
              (when (not (null? points))
                (cond
                 [(= (remainder index 3) 1)
                  (fill-points matrix (list (car points)) "red")]
                 [(= (remainder index 3) 2)
                  (fill-points matrix (list (car points)) 'pattern1)]
                 [(= (remainder index 3) 0)
                  (fill-points matrix (list (car points)) "yellow")])
                (loop (cdr points) (add1 index))))
            (draw matrix png_5X5_file 'png)
            (draw matrix svg_5X5_file 'svg)
            ))
        (lambda ()
          ;(void)
          (delete-file svg_5X5_file)
          (delete-file png_5X5_file)
          )))
   ))

(run-tests test-func)
