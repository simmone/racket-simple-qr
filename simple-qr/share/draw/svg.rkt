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
  (let* ([canvas_width (* (+ (QR-modules qr) (* QUIET_ZONE_WIDTH 2)) (QR-module_width qr))])
    (with-output-to-file
        file_name #:exists 'replace
        (lambda ()
          (printf "~a"
                  (svg-out
                   canvas_width canvas_width
                   (lambda ()
                     (let ([back_rect (svg-def-shape (new-rect canvas_width canvas_width))]
                           [back_sstyle (sstyle-new)]
                           [rect (svg-def-shape (new-rect (QR-module_width qr) (QR-module_width qr)))]
                           [front_sstyle (sstyle-new)])
                       
                       (set-SSTYLE-fill! back_sstyle (QR-zero_color qr))
                       (svg-place-widget back_rect #:style back_sstyle)
                       
                       (set-SSTYLE-fill! front_sstyle (QR-one_color qr))
                       (draw-points rect front_sstyle (QR-points_map qr) (QR-module_width qr))))))))))
