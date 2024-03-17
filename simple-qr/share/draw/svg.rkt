#lang racket

(require simple-svg)

(require "../lib.rkt")
(require "../qr.rkt")

(provide (contract-out
          [draw-svg (-> QR? path-string? void?)]
          ))

(define (draw-points rect points_color_map module_width)
  (let loop ([points_list
              (sort (hash->list points_map) (lambda (c d) (< (+ (caar c) (cdar c)) (+ (caar d) (cdar d)))))])
    (when (not (null? points_list))
          (when (= (cdar points_list) 1)
                (let ([new_point_pair (cons (+ (cdaar points_list) 4) (+ (caaar points_list) 4))])
                  (svg-place-widget
                   rect
                   #:style rect_sstyle
                   #:at (locate-brick module_width new_point_pair))))
          (loop (cdr points_list)))))

(define (draw-svg qr file_name)
  (let ([canvas_width (* (+ (QR-modules qr) (* QUIET_ZONE_WIDTH 2)) (QR-module_width qr))])
    (with-output-to-file
        file_name #:exists 'replace
        (lambda ()
          (printf "~a"
                  (svg-out
                   canvas_width canvas_width
                   (lambda ()
                     (let (
                           [brick_rect (svg-def-shape (new-rect qr_width qr_width))]
                           )

                       (draw-points brick_rect (QR-points_color_map qr) (QR-module_width qr))))))))))
