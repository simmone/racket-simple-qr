#lang racket

(require "lib.rkt"
         "capacity.rkt"
         "draw/matrix.rkt")

(define QUIET_ZONE_BRICKS 4)

(provide (contract-out
          [QR-MODE? (-> symbol? boolean?)]
          [QR-ERROR-LEVEL? (-> symbol? boolean?)]
          [struct QR
                  (
                   (data string?)
                   (mode QR-MODE?)
                   (error_level QR-ERROR-LEVEL?)
                   (version natural?)
                   (modules natural?)
                   (point_val_map (hash/c (cons/c natural? natural?) (or/c 0 1)))
                   (point_type_map (hash/c (cons/c natural? natural?) (or/c 'finder 'separator 'timing 'alignment 'dark 'format 'version 'data)))
                   (matrix MATRIX?)
                   (one_color string?)
                   (zero_color (or/c string? 'transparent))
                   )
                  ]
          [new-qr (-> string? natural? QR-MODE? QR-ERROR-LEVEL? string? string? QR?)]
          [new-default-qr (-> string? QR?)]
          [version->modules (-> natural? natural?)]
          [QUIET_ZONE_BRICKS natural?]
          [add-point (-> (cons/c natural? natural?) (or/c 0 1) (or/c 'finder 'separator 'timing 'alignment 'dark 'format 'version 'data) QR? void?)]
          [add-raw-point (-> (cons/c natural? natural?) (or/c 0 1) (or/c 'finder 'separator 'timing 'alignment 'dark 'format 'version 'data) QR? void?)]
          [fill-type-points (-> (or/c 'finder 'separator 'timing 'alignment 'dark 'format 'version 'data 'all) (cons/c string? string?) QR? void?)]
          [add-quiet-zone-offset (-> (cons/c natural? natural?) (cons/c natural? natural?))]
          [get-version (-> natural? QR-MODE? QR-ERROR-LEVEL? natural?)]
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

(define (QR-MODE? mode)
  (if (not (memq mode '(A N B))) #f #t))

(define (QR-ERROR-LEVEL? error_level)
  (if (not (memq error_level '(L M Q H))) #f #t))

(define (new-qr data module_width mode error_level one_color zero_color)
  (let* ([version (get-version (string-length data) mode error_level)]
         [modules (version->modules version)]
         [canvas_modules (+ modules (* QUIET_ZONE_BRICKS 2))]
         [matrix (new-matrix canvas_modules module_width)]
         [qr (QR data mode error_level version modules (make-hash) (make-hash) matrix one_color zero_color)])
    qr))

(define (new-default-qr data)
  (new-qr data 20 'B 'H "black" "white"))

(define (version->modules version)
  (if (and (>= version 1) (<= version 40))
      (+ 21 (* 4 (sub1 version)))
      (error "invalid version!")))

(define (add-point point val type qr)
  (let ([new_point
         (cons
          (+ QUIET_ZONE_BRICKS (car point))
          (+ QUIET_ZONE_BRICKS (cdr point)))])
    (add-raw-point new_point val type qr)))

(define (add-raw-point point val type qr)
  (hash-set! (QR-point_val_map qr) point val)
  (hash-set! (QR-point_type_map qr) point type))

(define (fill-type-points type color_pair qr)
  (let loop ([type_points
              (filter
               (lambda (point)
                 (or (eq? type 'all) (eq? (hash-ref (QR-point_type_map qr) point) type)))
               (hash-keys (QR-point_val_map qr)))])
    (when (not (null? type_points))
      (if (= (hash-ref (QR-point_val_map qr) (car type_points)) 1)
          (fill-point-color (QR-matrix qr) (car type_points) (car color_pair))
          (fill-point-color (QR-matrix qr) (car type_points) (cdr color_pair)))
      (loop (cdr type_points)))))

(define (get-version char_count mode error_level)
  (let loop ([loop_list (hash-ref *capacity_table* (format "~a-~a" mode error_level))])
    (if (not (null? loop_list))
        (if (<= char_count (cdar loop_list))
            (caar loop_list)
            (loop (cdr loop_list)))
        (error (format "no such version: mode[~a]error_level[~a]char_count[~a]" mode error_level char_count)))))

(define (add-quiet-zone-offset point)
  (cons
   (+ (car point) QUIET_ZONE_BRICKS)
   (+ (cdr point) QUIET_ZONE_BRICKS)))
