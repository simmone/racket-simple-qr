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
                  [color_style_map (make-hash)]
                  [pattern1_id (def-pattern1 (MATRIX-brick_width matrix))])
              
              (for-each
               (lambda (color)
                 (hash-set! color_style_map color #f))
               (hash-values (MATRIX-points_color_map matrix)))

              (for-each
               (lambda (color)
                 (when (string? color)
                   (let ([sstyle (sstyle-new)])
                     (set-SSTYLE-fill! sstyle color)
                     (hash-set! color_style_map color sstyle))))
               (hash-keys color_style_map))

              (let loop ([points (MATRIX-points matrix)])
                (when (not (null? points))
                  (let* ([point (car points)]
                         [color (hash-ref (MATRIX-points_color_map matrix) point #f)]
                         [style (hash-ref color_style_map color #f)])

                    (when color
                      (cond
                       [(eq? color 'pattern1)
                        (svg-place-widget
                         pattern1_id
                         #:at (locate-brick (MATRIX-brick_width matrix) point))]
                       [else
                        (svg-place-widget
                         basic_brick
                         #:style style
                         #:at (locate-brick (MATRIX-brick_width matrix) point))])))
                  (loop (cdr points)))))))))))

(define (def-pattern1 brick_width)
  (let* (
         [step 20]
         [rect_id (svg-def-shape (new-rect brick_width brick_width))]
         [cross_line1_id (svg-def-shape (new-line (cons step 0) (cons 0  brick_width)))]
         [cross_line2_id (svg-def-shape (new-line '(0 . 0) (cons step brick_width)))]
         [rect_sstyle (sstyle-new)]
         [group_sstyle (sstyle-new)]
         [cross_line_id #f]
         [pattern_id #f]
        )

    (set-SSTYLE-stroke-width! group_sstyle 1)
    (set-SSTYLE-stroke! group_sstyle "black")
    (set! cross_line_id
          (svg-def-group
           (lambda ()
             (svg-place-widget cross_line1_id #:style group_sstyle)
             (svg-place-widget cross_line2_id #:style group_sstyle)
             )))

    (set-SSTYLE-stroke-width! rect_sstyle 1)
    (set-SSTYLE-stroke! rect_sstyle "black")
    (set-SSTYLE-fill! rect_sstyle "white")
    (set! pattern_id
          (svg-def-group
           (lambda ()
             (svg-place-widget rect_id #:style rect_sstyle)
             (let draw-cross-line ([x 0])
               (when (< x brick_width)
                 (svg-place-widget cross_line_id #:at (cons x 0))
                 (draw-cross-line (+ x step)))))))
    pattern_id))

