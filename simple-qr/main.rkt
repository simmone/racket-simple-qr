#lang racket

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

(let* ([data "HELLO WORLD"]
       [mode "A"]
       [error_level "Q"]
       [version (get-version data mode error_level)]
       [modules (version->modules version)]
       [module_width 10]
       )

  (let* ([points_map (make-hash)]
         [sum_count (* modules modules)]
         [trace_count (hash-count points_map)])

    (printf "start version:~a modules:~a sum:~a\n" version modules sum_count)

    (draw-finder-pattern modules points_map)
    ; (printf "finder-pattern:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map)))
    (set! trace_count (hash-count points_map))

    (draw-separator modules points_map)
    ; (printf "separator:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map)))
    (set! trace_count (hash-count points_map))

    (draw-timing-pattern modules points_map)
    ; (printf "timing-pattern:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map)))
    (set! trace_count (hash-count points_map))

    (draw-alignment-pattern version points_map)
    ; (printf "alignment-pattern:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map)))
    (set! trace_count (hash-count points_map))

    (draw-reserved-format-information modules points_map)
    ; (printf "format-information:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map)))
    (set! trace_count (hash-count points_map))

    (draw-reserved-version-information version modules points_map)
    ; (printf "version-information:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map)))
    (set! trace_count (hash-count points_map))

    (draw-dark-module version points_map)
    ; (printf "dark-module:~a remain:~a\n" (- (hash-count points_map) trace_count) (- sum_count (hash-count points_map)))
    (set! trace_count (hash-count points_map))

    (let ([data_list (string->list (matrix-data data #:mode mode #:error_level error_level))]
          [trace_list (snake-modules modules #:skip_points_hash points_map)])
      ; (printf "data_length:~a trace_length:~a\n" (length data_list) (length trace_list))
      (draw-data data_list trace_list points_map))

    (let ([mask_number (mask-data modules points_map)])
      (draw-format-information error_level mask_number modules points_map))

    (draw-version-information version modules points_map)

    (let* ([canvas_width (* (+ modules 8) module_width)]
           [target (make-bitmap canvas_width canvas_width)]
           [dc (new bitmap-dc% [bitmap target])])

      (draw-background dc (+ modules 8) module_width)

      (draw-points dc module_width points_map)

      (send target save-file "box.png" 'png)
      
      (system "open box.png"))))


