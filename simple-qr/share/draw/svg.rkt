#lang racket

(require simple-svg)

(require "../lib.rkt")
(require "../qr.rkt")

(provide (contract-out
          [draw-svg (-> QR? path-string? void?)]
          ))

(define (draw-svg qr file_name)
  (with-output-to-file
      file_name #:exists 'replace
      (lambda ()
        (printf
         "~a"
         (svg-out
          (QR-canvas_width qr) (QR-canvas_width qr)
          (lambda ()
            (let ([basic_brick (svg-def-shape (new-rect (QR-module_width qr) (QR-module_width qr)))]
                  [color_style_map (make-hash)])
              
              (for-each
               (lambda (color)
                 (hash-set! color_style_map color #f))
               (hash-values (QR-points_color_map qr)))

              (for-each
               (lambda (color)
                 (let ([sstyle (new-sstyle)])
                   (set-SSTYLE-fill! _sstyle color)
                   (hash-set! color_style_map color sstyle)))
               (hash-keys color_style_map))

              (let loop ([points (QR-points qr)])
                (when (not (null? points))
                  (let* ([point (car points)]
                         [color (hash-ref (QR-points_color_map qr) point )]
                         [style (hash-ref color_style_map color)])

                      (svg-place-widget
                       basic_brick
                       #:style style
                       #:at (locate-brick module_width point)))
                  (loop (cdr points)))))))))))
