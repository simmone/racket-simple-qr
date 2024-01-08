#lang racket

(require simple-svg)

(require "lib.rkt")
(require "canvas.rkt")

(provide (contract-out
          [draw-svg (-> CANVAS? path-string? void?)]
          ))

(define (draw-points points_map module_width)
  (let ([rect (svg-def-rect module_width module_width)])

    (let loop ([points
                (sort (hash-keys points_map) (lambda (c d) (< (+ (car c) (cdr c)) (+ (car d) (cdr d)))))])
      (when (not (null? points))
        (let ([point_color (hash-ref points_map (car points))])
          (if (string=? point_color "pattern")
              (let (
                    [line1 (svg-def-line '(10 . 0) (cons 0  module_width))]
                    [line2 (svg-def-line '(0 . 0) (cons 10 module_width))]
                    [rect_sstyle (sstyle-new)]
                    [group_sstyle (sstyle-new)]
                    )

                (svg-def-group
                 "rect"
                 (lambda ()
                   (svg-use-shape rect (sstyle-new) #:at? '(0 . 0))))

                (sstyle-set! group_sstyle 'stroke-width 1)
                (sstyle-set! group_sstyle 'stroke "black")
                (svg-def-group
                 "pattern"
                 (lambda ()
                   (svg-use-shape line1 group_sstyle #:at? '(0 . 0))
                   (svg-use-shape line2 group_sstyle #:at? '(0 . 0))))

                (svg-show-group "rect")
                (svg-show-group "pattern" #:at? '(0 . 0)))
              (let ([module_style (sstyle-new)])
                (sstyle-set! module_style 'fill (hash-ref points_map (car points)))
                (svg-use-shape
                 rect
                 module_style
                 #:at? (locate-brick module_width (car points))))))
        (loop (cdr points))))))

(define (draw-svg canvas file_name)
  (let* ([canvas_width (* (CANVAS-modules canvas) (CANVAS-module_width canvas))]
         [svg_str
          (svg-out
           canvas_width
           canvas_width
           (lambda ()
             (let ([back_rect (svg-def-rect canvas_width canvas_width)]
                   [back_sstyle (sstyle-new)])
               
               (sstyle-set! back_sstyle 'fill (CANVAS-background_color canvas))
               (svg-use-shape back_rect back_sstyle)
               
               (draw-points (CANVAS-points_map canvas) (CANVAS-module_width canvas))
               
               (svg-show-default))))])

    (with-output-to-file
        file_name #:exists 'replace
        (lambda ()
          (printf "~a" svg_str)))))
