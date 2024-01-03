#lang racket

(require racket/draw)

(require "lib.rkt")
(require "canvas.rkt")

(require "../func.rkt")

(provide (contract-out
          [draw-png (-> CANVAS? path-string? (or/c 'png 'jpeg 'bmp) void?)]
          ))

(define (draw-module dc point_color canvas_fg_color canvas_bg_color place_pair module_width)
    (when (not (string=? point_color "transparent"))
      (if (string=? point_color "pattern")
          (let ([space 10])
            (send dc set-pen (hex_color->racket_color canvas_fg_color) 1 'solid)
            (send dc set-brush (hex_color->racket_color canvas_bg_color) 'solid)
            (send dc draw-rectangle (cdr place_pair) (car place_pair) module_width module_width)

            (send dc set-pen (hex_color->racket_color canvas_fg_color) 1 'solid)
            (let loop ([loop_col (+ (cdr place_pair) space)])
              (when (<= loop_col (+ (cdr place_pair) module_width))
                (send dc draw-line
                      loop_col (car place_pair)
                      (- loop_col space) (+ (car place_pair) module_width))

                (send dc draw-line
                      loop_col (+ (car place_pair) module_width)
                      (- loop_col space) (car place_pair))
                (loop (+ loop_col space))))
            )
          (begin
            (send dc set-pen (hex_color->racket_color point_color) 1 'solid)
            (send dc set-brush (hex_color->racket_color point_color) 'solid)

            (send dc draw-rectangle (cdr place_pair) (car place_pair) module_width module_width)))))

(define (draw-points dc module_width points_map fg_color bg_color)
  (let loop ([points_list
             (sort (hash-keys points_map) (lambda (c d) (< (+ (car c) (cdr c)) (+ (car d) (cdr d)))))])
    (when (not (null? points_list))
      (draw-module 
       dc
       (hash-ref points_map (car points_list))
       fg_color
       bg_color
       (locate-brick module_width (car points_list))
       module_width)
      (loop (cdr points_list)))))

(define (draw-png canvas file_name output_type)
  (let* ([canvas_width (* (CANVAS-modules canvas) (CANVAS-module_width canvas))]
         [target (make-bitmap canvas_width canvas_width)]
         [dc (new bitmap-dc% [bitmap target])])

     (send dc set-smoothing 'aligned)

     (when (not (string=? (CANVAS-background_color canvas) "transparent"))
           (send dc set-pen (hex_color->racket_color (CANVAS-background_color canvas)) 1 'solid)
           (send dc set-brush (hex_color->racket_color (CANVAS-background_color canvas)) 'solid)
           (send dc draw-rectangle 0 0 canvas_width canvas_width))

     (draw-points dc (CANVAS-module_width canvas) (CANVAS-points_map canvas) (CANVAS-foreground_color canvas) (CANVAS-background_color canvas))
    
     (send target save-file file_name output_type)
     
     (void)))
