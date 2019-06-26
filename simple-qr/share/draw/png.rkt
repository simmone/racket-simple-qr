#lang racket

(require racket/draw)

(require "lib.rkt")

(require "../func.rkt")

(provide (contract-out
          [draw-png (-> natural? natural? hash? hash? (cons/c string? string?) path-string? void?)]
          ))

(define (draw-module dc color place_pair module_width)
  (when (not (string=? color "transparent"))
        (send dc set-pen (hex_color->racket_color color) 1 'solid)
        (send dc set-brush (hex_color->racket_color color) 'solid)

        (send dc draw-rectangle (cdr place_pair) (car place_pair) module_width module_width)))

(define (draw-points dc module_width points_map color_map front_color)
  (let loop ([points_list
              (sort (hash->list points_map) (lambda (c d) (< (+ (caar c) (cdar c)) (+ (caar d) (cdar d)))))])
    (when (not (null? points_list))
          (when (string=? (cdar points_list) "1")
           (let ([new_point_pair (cons (+ (cdaar points_list) 4) (+ (caaar points_list) 4))])
             (draw-module 
              dc
              (hash-ref color_map (caar points_list) front_color)
              (locate-brick module_width new_point_pair)
              module_width)))
          (loop (cdr points_list)))))

(define (draw-png modules module_width points_map color_map color file_name)
  (let* ([canvas_width (* (+ modules 8) module_width)]
         [target (make-bitmap canvas_width canvas_width)]
         [dc (new bitmap-dc% [bitmap target])])

     (send dc set-smoothing 'aligned)

     (when (not (string=? (cdr color) "transparent"))
           (send dc set-pen (hex_color->racket_color (cdr color)) 1 'solid)
           (send dc set-brush (hex_color->racket_color (cdr color)) 'solid)
           (send dc draw-rectangle 0 0 canvas_width canvas_width))

     (draw-points dc module_width points_map color_map (car color))
    
     (send target save-file file_name 'png)
     
     (void)))
