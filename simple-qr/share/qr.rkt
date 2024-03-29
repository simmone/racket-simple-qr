#lang racket

(require "lib.rkt"
         "version.rkt"
         "draw/matrix.rkt")

(define QUIET_ZONE_WIDTH 4)

(provide (contract-out
          [struct QR
                  (
                   (data string?)
                   (mode string?)
                   (error_level string?)
                   (version natural?)
                   (modules natural?)
                   (point_val_map (hash/c (cons/c natural? natural?) (or/c 0 1)))
                   (point_type_map (hash/c (cons/c natural? natural?) string?))
                   (matrix MATRIX?)
                   (one_color string?)
                   (zero_color (or/c string? 'transparent))
                   )
                  ]
          [new-qr (-> string? natural? string? string? string? string? QR?)]
          [new-default-qr (-> string? QR?)]
          [version->modules (-> natural? natural?)]
          [QUIET_ZONE_WIDTH natural?]
          [add-point (-> (cons/c natural? natural?) (or/c 0 1) string? QR? void?)]
          ))

(struct QR
        (
         (data #:mutable)
         (mode #:mutable)
         (error_level #:mutable)
         (version #:mutable)
         (modules #:mutable)
         (point_val_map #:mutable)
         (point_type_map #:mutable)
         (matrix #:mutable)
         (one_color #:mutable)
         (zero_color #:mutable)
         )
        #:transparent
        )

(define (new-qr data module_width mode error_level one_color zero_color)
  (let* ([version (get-version (string-length data) mode error_level)]
         [modules (version->modules version)]
         [canvas_modules (+ modules (* QUIET_ZONE_WIDTH 2))]
         [matrix (new-matrix canvas_modules module_width)]
         [qr (QR data mode error_level version modules (make-hash) (make-hash) matrix one_color zero_color)])
    qr))

(define (new-default-qr data)
  (new-qr data 20 "B" "H" "black" "white"))

(define (version->modules version)
  (if (and (>= version 1) (<= version 40))
      (+ 21 (* 4 (sub1 version)))
      (error "invalid version!")))

(define (add-point point val type qr)
  (hash-set! (QR-point_val_map qr) point val)
  (hash-set! (QR-point_type_map qr) point type))
