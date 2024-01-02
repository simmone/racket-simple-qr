#lang racket

;; points_map: point pair => color
;; point pair: '(1 . 1)
;; color: FF00FF or red or transparent
(provide (contract-out
          [struct CANVAS
                  (
                   (modules natural?)
                   (module_width natural?)
                   (points_map (hash/c (cons/c natural? natural?) string?))
                   (foreground_color string?)
                   (background_color string?)
                   )
                  ]
          [init-color (-> CANVAS? string? void?)]
          ))

(struct CANVAS
        (
         (modules #:mutable)
         (module_width #:mutable)
         (points_map #:mutable)
         (foreground_color #:mutable)
         (background_color #:mutable)
         )
        #:transparent
        )

(define (init-color canvas color)
  (let loop-row ([loop_row 1])
    (when (<= loop_row (CANVAS-modules canvas))
      (let loop-col ([loop_col 1])
        (when (<= loop_col (CANVAS-modules canvas))
          (hash-set! (CANVAS-points_map canvas) (cons loop_row loop_col) color)
          (loop-col (add1 loop_col))))
      (loop-row (add1 loop_row)))))
