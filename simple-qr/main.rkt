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
(require "lib/lib.rkt")
(require "lib/func/func.rkt")

(require racket/draw)

(let* ([data "http://chenxiao.info"]
       [mode "B"]
       [error_level "H"]
       [version (get-version data mode error_level)]
       [modules (version->modules version)]
       [module_width 10]
       [canvas_width (* modules module_width)]
       )

  (define target (make-bitmap canvas_width canvas_width))

  (define dc (new bitmap-dc% [bitmap target]))

  ;; draw the background, help to count module
  (let loop-row ([row 1])
    (when (<= row modules)
          (let loop-col ([col 1])
            (when (<= col modules)
                  (if (= (remainder col 2) 1)
                      (draw-module dc "pink" (locate-brick module_width (cons row col)) module_width)
                      (draw-module dc "orchid" (locate-brick module_width (cons row col)) module_width))
                  (loop-col (add1 col))))

          (let loop-col ([col 1])
            (when (<= col modules)
                  (when (= (remainder row 2) 0)
                        (draw-module dc "orchid" (locate-brick module_width (cons row col)) module_width))
                  (loop-col (add1 col))))

            (loop-row (add1 row))))

  (let ([points_exists_map (make-hash)])
    (draw-finder-pattern dc modules module_width points_exists_map)

    (draw-separator dc modules module_width points_exists_map)

    (draw-timing-pattern dc modules module_width points_exists_map)

    (draw-format-information dc modules module_width points_exists_map)

    (draw-version-information dc version modules module_width points_exists_map)

    (draw-alignment-pattern dc version module_width points_exists_map)

    (draw-dark-module dc version module_width points_exists_map)
    
    (let ([data_list (string->list (matrix-data data #:mode mode #:error_level error_level))]
          [trace_list (snake-modules modules #:skip_points_hash points_exists_map)])
      (draw-data dc module_width data_list trace_list points_exists_map))
    )

  (send target save-file "box.png" 'png)
  
  (system "open box.png")
)
