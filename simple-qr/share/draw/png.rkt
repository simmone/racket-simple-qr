#lang racket

(require racket/draw)

(require "lib.rkt")

(provide (contract-out
          [draw-png (-> natural? natural? hash? hash? path-string? void?)]
          ))

(define (draw-module dc color place_pair module_width)
  (when (not (string=? color "transparent"))
        (send dc set-pen color 1 'solid)
        (send dc set-brush color 'solid)

        (send dc draw-rectangle (cdr place_pair) (car place_pair) module_width module_width)))

(define (draw-points dc module_width points_map color_map)
  (hash-for-each
   points_map
   (lambda (point_pair val)
     (let ([new_point_pair (cons (+ (cdr point_pair) 4) (+ (car point_pair) 4))])
       (draw-module 
        dc
        (hash-ref color_map point_pair (if (string=? (~a val) "1") "black" "white"))
        (locate-brick module_width new_point_pair)
        module_width)))))

(define (draw-background dc modules module_width)
  (let loop-row ([row 1])
    (when (<= row modules)
          (let loop-col ([col 1])
            (when (<= col modules)
                  (draw-module dc "white" (locate-brick module_width (cons row col)) module_width)
                  (loop-col (add1 col))))
          (loop-row (add1 row)))))

(define (draw-png modules module_width points_map color_map file_name)
  (let* ([canvas_width (* (+ modules 8) module_width)]
         [target (make-bitmap canvas_width canvas_width)]
         [dc (new bitmap-dc% [bitmap target])])

     (send dc set-smoothing 'aligned)

     (draw-background dc (+ modules 8) module_width)

     (draw-points dc module_width points_map color_map)
    
     (send target save-file file_name 'png)
     
     (void)))
