#lang racket

(require "../../../../racket-simple-svg/simple-svg/main.rkt")

(require "lib.rkt")

(provide (contract-out
          [draw-svg (-> natural? natural? hash? hash? path-string? void?)]
          ))

(define (draw-module rect rect_style color place_pair)
  (when (string=? color "black")
    (svg-use-shape rect rect_style #:at? place_pair)))

(define (draw-points rect rect_sstyle points_map color_map module_width)
  (hash-for-each
   points_map
   (lambda (point_pair val)
     (let ([new_point_pair (cons (+ (cdr point_pair) 4) (+ (car point_pair) 4))])
       (draw-module 
        rect rect_sstyle
        (hash-ref color_map point_pair (if (string=? (~a val) "1") "black" "white"))
        (locate-brick module_width new_point_pair))))))

(define (draw-svg modules module_width points_map color_map file_name)
  (let* ([canvas_width (* (+ modules 8) module_width)])
    (with-output-to-file
        file_name #:exists 'replace
        (lambda ()
          (printf "~a"
                  (svg-out
                   canvas_width canvas_width
                   (lambda ()
                     (let ([back_rect (svg-def-rect canvas_width canvas_width)]
                           [back_sstyle (sstyle-new)]
                           [rect (svg-def-rect module_width module_width)]
                           [rect_sstyle (sstyle-new)])
                       
                       (set-sstyle-fill! back_sstyle "white")
                       (svg-use-shape back_rect back_sstyle)
                       
                       (set-sstyle-fill! rect_sstyle "black")

                       (draw-points rect rect_sstyle points_map color_map module_width)
                       
                       (svg-show-default)))))))))
