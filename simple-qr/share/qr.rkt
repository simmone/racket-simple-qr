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
                   (points_val_map (hash/c (cons/c natural? natural?) (or/c 0 1)))
                   (one_color string?)
                   (zero_color (or/c string? 'transparent))
                   )
                  ]
          [new-qr (-> string? natural? string? string? string? string? QR?)]
          [new-default-qr (-> string? QR?)]
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
         (points_val_map #:mutable)
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
         [qr (QR data mode error_level version modules (make-hash) one_color zero_color)])
    qr))

(define (new-default-qr data)
  (new-qr data 20 "B" "H" "black" "white"))

(define (version->modules version)
  (if (and (>= version 1) (<= version 40))
      (+ 21 (* 4 (sub1 version)))
      (error "invalid version!")))
