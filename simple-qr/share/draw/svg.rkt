#lang racket

(require simple-svg)

(require "../lib.rkt")
(require "../qr.rkt")

(provide (contract-out
          [draw-svg (-> QR? path-string? void?)]
          ))

(define (draw-points rect rect_sstyle points_map module_width)
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
  (let* ([canvas_width (* (+ (QR-modules qr) (* QUIET_ZONE_WIDTH 2)) (QR-module_width qr))]
         [qr_width (* (QR-modules qr) (QR-module_width qr))])
    (with-output-to-file
        file_name #:exists 'replace
        (lambda ()
          (printf "~a"
                  (svg-out
                   canvas_width canvas_width
                   (lambda ()
                     (let (
                           [canvas_rect (svg-def-shape (new-rect canvas_width canvas_width))]
                           [canvas_sstyle (sstyle-new)]
                           [canvas_color
                            (hash-ref (QR-type_color_map qr) "canvas" (QR-zero_color qr))]

                           [qr_canvas_rect (svg-def-shape (new-rect qr_width qr_width))]
                           [qr_canvas_sstyle (sstyle-new)]
                           [qr_canvas_color
                            (hash-ref (QR-type_color_map qr) "qr_canvas" (QR-zero_color qr))]

                           [front_sstyle (sstyle-new)]
                           [qr_rect (svg-def-shape (new-rect qr_width qr_width))]
                           )

                       (set-SSTYLE-fill! canvas_sstyle canvas_color)
                       (svg-place-widget canvas_rect #:style canvas_sstyle)

                       (set-SSTYLE-fill! qr_canvas_sstyle qr_canvas_color)
                       (svg-place-widget
                        qr_canvas_rect
                        #:at (locate-brick (QR-module_width qr) (cons QUIET_ZONE_WIDTH QUIET_ZONE_WIDTH))
                        #:style qr_canvas_sstyle)

                       (set-SSTYLE-fill! front_sstyle (QR-one_color qr))
                       (draw-points qr_rect front_sstyle (QR-points_map qr) (QR-module_width qr))))))))))
