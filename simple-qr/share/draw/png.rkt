#lang racket

(require racket/draw)

(require "lib.rkt")

(require "../func.rkt")

(provide (contract-out
          [draw-png (-> natural? natural? (hash/c natural? natural?) path-string? void?)]
          ))

(define (draw-module dc color place_pair module_width)
  (when (not (string=? color "transparent"))
        (send dc set-pen (hex_color->racket_color color) 1 'solid)
        (send dc set-brush (hex_color->racket_color color) 'solid)

        (send dc draw-rectangle (cdr place_pair) (car place_pair) module_width module_width)))

(define (draw-points dc module_width points_map)
  (let loop ([points_list
             (sort (hash->keys points_map) (lambda (c d) (< (+ (car c) (cdr c)) (+ (car d) (cdr d)))))])
    (when (not (null? points_list))
      (draw-module 
       dc
       (hash-ref points_map (car points_list))
       (locate-brick module_width (car potins_list))
       module_width)))
  (loop (cdr points_list)))

(define (draw-png modules module_width points_map file_name)
  (let* ([canvas_width (* (+ modules 8) module_width)]
         [target (make-bitmap canvas_width canvas_width)]
         [dc (new bitmap-dc% [bitmap target])])

     (send dc set-smoothing 'aligned)

     (when (not (string=? (cdr color) "transparent"))
           (send dc set-pen (hex_color->racket_color (cdr color)) 1 'solid)
           (send dc set-brush (hex_color->racket_color (cdr color)) 'solid)
           (send dc draw-rectangle 0 0 canvas_width canvas_width))

     (draw-points dc module_width points_map)
    
     (send target save-file file_name 'png)
     
     (void)))
