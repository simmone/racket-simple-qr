#lang racket

(require simple-svg)

(require "lib.rkt")
(require "canvas.rkt")

(provide (contract-out
          [draw-svg (-> CANVAS? path-string? void?)]
          ))

(define (draw-points rect rect_sstyle points_map module_width)
  (let loop ([points_list
              (sort (hash->list points_map) (lambda (c d) (< (+ (caar c) (cdar c)) (+ (caar d) (cdar d)))))])
    (when (not (null? points_list))
          (when (string=? (cdar points_list) "1")
                (let ([new_point_pair (cons (+ (cdaar points_list) 4) (+ (caaar points_list) 4))])
                  (svg-place-widget
                   rect
                   #:style rect_sstyle
                   #:at (locate-brick module_width new_point_pair))))
          (loop (cdr points_list)))))

(define (draw-svg canvas file_name)
  (let* ([canvas_width (* (+ (CANVAS-modules canvas) 8) (CANVAS-module_width canvas))])
    (with-output-to-file
        file_name #:exists 'replace
        (lambda ()
          (printf "~a"
                  (svg-out
                   canvas_width canvas_width
                   (lambda ()
                     (let ([back_rect (svg-def-shape (new-rect canvas_width canvas_width))]
                           [back_sstyle (sstyle-new)]
                           [rect (svg-def-shape (new-rect (CANVAS-module_width canvas) (CANVAS-module_width canvas)))]
                           [front_sstyle (sstyle-new)])
                       
                       (set-SSTYLE-fill! back_sstyle (CANVAS-background_color canvas))
                       (svg-place-widget back_rect #:style back_sstyle)
                       
                       (set-SSTYLE-fill! front_sstyle (CANVAS-foreground_color canvas))
                       (draw-points rect front_sstyle (CANVAS-points_map canvas) (CANVAS-module_width canvas))))))))))
