#lang racket

(require "lib.rkt")
(require "version.rkt")

(define QUIET_ZONE_WIDTH 4)

;; points_map: point pair => color
;; point pair: '(1 . 1)
;; color: "FF00FF" or "red" or 'transparent
;; one_color: value 1's default color
;; zero_color: value 0's default color
(provide (contract-out
          [struct QR
                  (
                   (data string?)
                   (mode string?)
                   (error_level string?)
                   (version natural?)
                   (modules natural?)
                   (module_width natural?)
                   (canvas_width natural?)
                   (points (listof (cons/c natural? natural?)))
                   (points_val_map (hash/c (cons/c natural? natural?) (or/c 0 1)))
                   (points_color_map (hash/c (cons/c natural? natural?) (or/c string? 'transparent))
                   (one_color string?)
                   (zero_color (or/c string? 'transparent))
                   )
                  ]
          [new-qr (-> string? natural? string? string? string? string? QR?)]
          [new-default-qr (-> string? QR?)]
          [fill-points (-> QR? (listof (cons/c natural? natural?)) (or/c 0 1) string? void?)]
          [version->modules (-> natural? natural?)]
          [QUIET_ZONE_WIDTH natural?]
          ))

(struct QR
        (
         (data #:mutable)
         (mode #:mutable)
         (error_level #:mutable)
         (version #:mutable)
         (modules #:mutable)
         (module_width #:mutable)
         (canvas_width #:mutable)
         (points #:mutable)
         (points_val_map #:mutable)
         (points_color_map #:mutable)
         (one_color #:mutable)
         (zero_color #:mutable)
         )
        #:transparent
        )

(define (new-qr data module_width mode error_level one_color zero_color)
  (let* ([version (get-version (string-length data) mode error_level)]
         [modules (version->modules version)]
         [canvas_width (+ modules (* QUIET_ZONE_WIDTH 2))]
         [points (get-points-between '(0 . 0) (cons (sub1 canvas_width) (sub1 canvas_width)) #:direction 'cross)]
         [qr (QR data mode error_level version modules module_width canvas_width points (make-hash) (make-hash) one_color zero_color)])
    
    (fill-points qr points 0 zero_color)

    qr))

(define (new-default-qr data)
  (new-qr data 20 "B" "H" "black" "white"))

(define (version->modules version)
  (if (and (>= version 1) (<= version 40))
      (+ 21 (* 4 (sub1 version)))
      (error "invalid version!")))

(define (fill-points qr points val color)
  (for-each
   (lambda (point)
     (hash-set! (QR-points_val_map qr) point val)
     (hash-set! (QR-points_color_map qr) point color))
   points))
