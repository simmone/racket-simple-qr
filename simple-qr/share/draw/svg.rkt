#lang racket

(require simple-svg)

(require "lib.rkt")

(provide (contract-out
          [draw-svg (-> natural? natural? hash? hash? (cons/c string? string?) path-string? void?)]
          ))

(define (draw-points rect rect_sstyle points_map color_map module_width)
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

(define (draw-svg modules module_width points_map color_map color file_name)
  (let* ([canvas_width (* (+ modules 8) module_width)])
    (with-output-to-file
        file_name #:exists 'replace
        (lambda ()
          (printf "~a"
                  (svg-out
                   canvas_width canvas_width
                   (lambda ()
                     (let ([back_rect (svg-def-shape (new-rect canvas_width canvas_width))]
                           [back_sstyle (sstyle-new)]
                           [rect (svg-def-shape (new-rect module_width module_width))]
                           [front_sstyle (sstyle-new)])
                       
                       (set-SSTYLE-fill! back_sstyle (cdr color))
                       (svg-place-widget back_rect #:style back_sstyle)
                       
                       (set-SSTYLE-fill! front_sstyle (car color))
                       (draw-points rect front_sstyle points_map color_map module_width)))))))))
