#lang racket

(provide (contract-out
          [qr-code (->* (string?) (#:mode string? #:error_level string? #:module_width exact-nonnegative-integer?) void?)]
          ))

(require "lib/finder-pattern/finder-pattern.rkt")
(require "lib/separator/separator.rkt")
(require "lib/format-information/format-information.rkt")
(require "lib/version-information/version-information.rkt")
(require "lib/timing-pattern/timing-pattern.rkt")
(require "lib/alignment-pattern/alignment-pattern.rkt")
(require "lib/dark-module/dark-module.rkt")
(require "lib/data-encoding/data-encoding.rkt")
(require "lib/fill-data/fill-data.rkt")
(require "lib/mask-data/mask-data.rkt")
(require "lib/lib.rkt")
(require "lib/func/func.rkt")

(require racket/draw)

(define (qr-code data #:mode [mode "B"] #:error_level [error_level "M"] #:module_width [module_width 10])
  (let* ([version (get-version data mode error_level)]
         [modules (version->modules version)])

    (trace (format "start version:~a modules:~a\n" version modules) 1)

    (let* ([points_map (make-hash)]
           [sum_count (* modules modules)]
           [trace_count (hash-count points_map)])

      (draw-finder-pattern modules points_map)
      (trace (format "finder-pattern:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map))) 1)
      (set! trace_count (hash-count points_map))

      (draw-separator modules points_map)
      (trace (format "separator:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map))) 1)
      (set! trace_count (hash-count points_map))

      (draw-timing-pattern modules points_map)
      (trace (format "timing-pattern:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map))) 1)
      (set! trace_count (hash-count points_map))

      (draw-alignment-pattern version points_map)
      (trace (format "alignment-pattern:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map))) 1)
      (set! trace_count (hash-count points_map))

      (draw-reserved-format-information modules points_map)
      (trace (format "format-information:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map))) 1)
      (set! trace_count (hash-count points_map))

      (draw-reserved-version-information version modules points_map)
      (trace (format "version-information:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map))) 1)
      (set! trace_count (hash-count points_map))

      (draw-dark-module version points_map)
      (trace (format "dark-module:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map))) 1)
      (set! trace_count (hash-count points_map))

      (let ([data_list (string->list (matrix-data data #:version version #:mode mode #:error_level error_level))]
            [trace_list (snake-modules modules #:skip_points_hash points_map)])
        (trace (format "data_length:~a trace_length:~a\n" (length data_list) (length trace_list)) 1)
        (draw-data data_list trace_list points_map))

      (trace (format "[21,21][21,20][20,21][20,20][19,21][19,20][18,21][18,20]=[~a~a~a~a~a~a~a~a]\n"
                     (car (hash-ref points_map '(21 . 21)))
                     (car (hash-ref points_map '(21 . 20)))
                     (car (hash-ref points_map '(20 . 21)))
                     (car (hash-ref points_map '(20 . 20)))
                     (car (hash-ref points_map '(19 . 21)))
                     (car (hash-ref points_map '(19 . 20)))
                     (car (hash-ref points_map '(18 . 21)))
                     (car (hash-ref points_map '(18 . 20))))
             1)

      (let ([mask_number (mask-data modules points_map)])
        (trace (format "mask_number:~a\n" mask_number) 1)
        (draw-format-information error_level mask_number modules points_map))

      (draw-version-information version modules points_map)

      (let* ([canvas_width (* (+ modules 8) module_width)]
             [target (make-bitmap canvas_width canvas_width)]
             [dc (new bitmap-dc% [bitmap target])])

        (draw-background dc (+ modules 8) module_width)

        (draw-points dc module_width points_map)

        (send target save-file "box.png" 'png)
        
        (system "open box.png")))))

(parameterize ([*trace_level* 1])
              (qr-code "01234567" #:mode "N" #:error_level "M"))


