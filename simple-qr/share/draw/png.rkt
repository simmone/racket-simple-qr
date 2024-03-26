#lang racket

(require racket/draw
         "../lib.rkt"
         "matrix.rkt")

(provide (contract-out
          [draw-png (-> MATRIX? path-string? (or/c 'png 'jpeg 'bmp) void?)]
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

(define (draw-png matrix file_name output_type)
  (let* ([target (make-bitmap (MATRIX-width matrix) (MATRIX-width matrix))]
         [dc (new bitmap-dc% [bitmap target])])

    (send dc set-smoothing 'aligned)

    (send dc set-pen (hex_color->racket_color "000000") 1 'solid)
    (send dc set-brush (hex_color->racket_color "000000") 'solid)
    (draw-points dc (MATRIX-brick_width matrix) (MATRIX-points_color_map matrix) "black" "white")
    
    (send target save-file file_name output_type)
    
    (void)))
