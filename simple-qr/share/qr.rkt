#lang racket

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
                   (points_val_map (hash/c (cons/c natural? natural?) (or/c 0 1)))
                   (points_color_map (hash/c (cons/c natural? natural?) string?))
                   (one_color string?)
                   (zero_color (or/c string? 'transparent))
                   )
                  ]
          [new-qr (-> string? natural? string? string? string? string? QR?)]
          [new-default-qr (-> string? QR?)]
          [add-point (-> (cons/c natural? natural?) (or/c 1 0) string? QR? void?)]
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
         (points_val_map #:mutable)
         (points_color_map #:mutable)
         (one_color #:mutable)
         (zero_color #:mutable)
         )
        #:transparent
        )

(define (version->modules version)
  (if (and (>= version 1) (<= version 40))
      (+ 21 (* 4 (sub1 version)))
      (error "invalid version!")))

(define (new-qr data module_width mode error_level one_color zero_color)
  (let* ([version (get-version (string-length data) mode error_level)]
         [modules (version->modules version)])
    (QR data mode error_level version modules module_width (make-hash) (make-hash) one_color zero_color)))

(define (new-default-qr data)
  (new-qr data 20 "B" "H" "black" "white"))

(define (add-point point val type qr)
  (hash-set! (QR-points_map qr) point val)
  (hash-set! (QR-type_points_map qr) type `(,@(hash-ref (QR-type_points_map qr) type '()) ,point)))
