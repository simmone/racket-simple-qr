#lang racket

(require simple-svg)

(require "../lib.rkt")
(require "../qr.rkt")

(provide (contract-out
          [draw-svg (-> QR? path-string? void?)]
          ))

(define (draw-points rect qr)

(define (draw-svg qr file_name)
  (with-output-to-file
      file_name #:exists 'replace
      (lambda ()
        (printf "~a"
                (svg-out
                 (QR-modules_with_quiet_zone qr) (QR-modules_with_quiet_zone qr)
                 (lambda ()
                   (let (
                         [basic_brick (svg-def-shape (new-rect (QR-modules_with_quiet_zone qr) (QR-modules_with_quiet_zone qr)))]
                         )

                     (let loop ([points 
                                 (get-points-between '(0 . 0) (cons (sub1 (QR-modules_with_quiet_zone qr)) (sub1 (QR-modules_with_quiet_zone qr))))])
                       (when (not (null? points))
                         (when (= (cdar points_list) 1)
                           (let ([new_point_pair (cons (+ (cdaar points_list) 4) (+ (caaar points_list) 4))])
                             (svg-place-widget
                              rect
                              #:style rect_sstyle
                              #:at (locate-brick module_width new_point_pair))))
                         (loop (cdr points_list)))))))))))
