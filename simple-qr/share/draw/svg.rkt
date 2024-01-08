#lang racket

(require simple-svg)

(require "lib.rkt")
(require "canvas.rkt")

(provide (contract-out
          [draw-svg (-> CANVAS? path-string? void?)]
          ))

(define (draw-points points_map module_width)
  (let ([rect (svg-def-rect module_width module_width)])

    (let loop ([points_list
                (sort (hash-keys points_map) (lambda (c d) (< (+ (car c) (cdr c)) (+ (car d) (cdr d)))))])
      (let ([module_style (sstyle-new)])
        (sstyle-set! module_style 'fill (hash-ref points_map (car points_list)))
        (when (not (null? points_list))
          (svg-use-shape
           rect
           module_style
           #:at? (locate-brick module_width (car points_list)))
          (loop (cdr points_list)))))))

(define (draw-svg canvas file_name)
  (let ([canvas_width (* (CANVAS-modules canvas) (CANVAS-module_width canvas))])
    (with-output-to-file
        file_name #:exists 'replace
        (lambda ()
          (printf "~a"
                  (svg-out
                   canvas_width
                   canvas_width
                   (lambda ()
                     (let ([back_rect (svg-def-rect canvas_width canvas_width)]
                           [back_sstyle (sstyle-new)])
                       
                       (sstyle-set! back_sstyle 'fill (CANVAS-background_color canvas))
                       (svg-use-shape back_rect back_sstyle)
                       
                       (draw-points (CANVAS-points_map canvas) (CANVAS-module_width canvas))
                       
                       (svg-show-default)))))))))
