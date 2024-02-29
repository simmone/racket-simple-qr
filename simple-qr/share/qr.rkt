#lang racket

(require "version.rkt")

;; points_map: point pair => color
;; point pair: '(1 . 1)
;; color: "FF00FF" or "red" or 'transparent
;; one_color: value 1's default color
;; zero_color: value 0's default color
(provide (contract-out
          [struct QR
                  (
                   (mode string?)
                   (error_level string?)
                   (modules natural?)
                   (module_width natural?)
                   (points_map (hash/c (cons/c natural? natural?) (or/c 1 0)))
                   (type_points_map (hash/c string? (listof (cons/c natural? natural?))))
                   (one_color string?)
                   (zero_color (or/c string? 'transparent))
                   )
                  ]
          [new-qr (-> string? string? string? string? QR?)]
          [add-point (-> (cons/c natural? natural?) (or/c 1 0) string? QR? void?)]
          [version->modules (-> natural? natural?)]
          ))

(struct QR
        (
         (mode #:mutable)
         (error_level #:mutable)
         (modules #:mutable)
         (module_width #:mutable)
         (points_map #:mutable)
         (type_points_map #:mutable)
         (one_color #:mutable)
         (zero_color #:mutable)
         )
        #:transparent
        )

(define (version->modules version)
  (if (and (>= version 1) (<= version 40))
      (+ 21 (* 4 (sub1 version)))
      (error "invalid version!")))

(define (new-qr mode error_level one_color zero_color)
  (let* ([version (get-version (string-length data) mode error_level)]
         [modules (version->modules version)])
    (QR mode error_level modules module_width (make-hash) (make-hash) one_color zero_color)))

(define (add-point point val type qr)
  (hash-set! (QR-points_map qr) point value)
  (hash-set! (QR-type_points_map qr) `(,@(hash-ref (QR-type_points_map qr) type '()) ,point)))
