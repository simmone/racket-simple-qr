#lang racket

(require simple-svg)

(require "matrix.rkt"
         "../lib.rkt")

(provide (contract-out
          [draw-svg (-> MATRIX? path-string? void?)]
          ))

(define (draw-svg matrix file_name)
  (with-output-to-file
      file_name #:exists 'replace
      (lambda ()
        (printf
         "~a"
         (svg-out
          (MATRIX-width matrix) (MATRIX-width matrix)
          (lambda ()
            (let ([basic_brick (svg-def-shape (new-rect (MATRIX-brick_width matrix) (MATRIX-brick_width matrix)))]
                  [color_style_map (make-hash)])
              
              (for-each
               (lambda (color)
                 (hash-set! color_style_map color #f))
               (hash-values (MATRIX-points_color_map matrix)))

              (for-each
               (lambda (color)
                 (let ([sstyle (sstyle-new)])
                   (set-SSTYLE-fill! sstyle color)
                   (hash-set! color_style_map color sstyle)))
               (hash-keys color_style_map))

              (let loop ([points (MATRIX-points matrix)])
                (when (not (null? points))
                  (let* ([point (car points)]
                         [color (hash-ref (MATRIX-points_color_map matrix) point )]
                         [style (hash-ref color_style_map color)])

                      (svg-place-widget
                       basic_brick
                       #:style style
                       #:at (locate-brick (MATRIX-brick_width matrix) point)))
                  (loop (cdr points)))))))))))
