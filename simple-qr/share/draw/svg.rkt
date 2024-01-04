#lang racket

(require simple-svg)

(require "lib.rkt")
(require "canvas.rkt")

(provide (contract-out
          [draw-svg (-> CANVAS? path-string? void?)]
          ))

(define (draw-points rect rect_sstyle points_map module_width)
  (let loop ([points_list
              (sort (hash-keys points_map) (lambda (c d) (< (+ (car c) (cdr c)) (+ (car d) (cdr d)))))])
    (when (not (null? points_list))
      (svg-use-shape
       rect
       rect_sstyle
       #:at? (locate-brick module_width (car points_list)))
      (loop (cdr points_list)))))

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
                           [back_sstyle (sstyle-new)]
                           [rect (svg-def-rect (CANVAS-module_width canvas) (CANVAS-module_width canvas))]
                           [front_sstyle (sstyle-new)])
                       
                       (sstyle-set! back_sstyle 'fill (CANVAS-background_color canvas))
                       (svg-use-shape back_rect back_sstyle)
                       
                       (sstyle-set! front_sstyle 'fill (CANVAS-foreground_color canvas))
                       (draw-points rect front_sstyle (CANVAS-points_map canvas) (CANVAS-module_width canvas))
                       
                       (svg-show-default)))))))))
