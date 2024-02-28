#lang racket

;; points_map: point pair => color
;; point pair: '(1 . 1)
;; color: "FF00FF" or "red" or 'transparent
(provide (contract-out
          [struct CANVAS
                  (
                   (modules natural?)
                   (module_width natural?)
                   (points_map (hash/c (cons/c natural? natural?) (or/c 1 0)))
                   (color_map (hash/c (cons/c natural? natural?) (cons/c string? string?)))
                   (foreground_color string?)
                   (background_color (or/c string? 'transparent))
                   )
                  ]
          [fill-color (-> CANVAS? (listof (cons/c natural? natural?)) string? void?)]
          ))

(struct CANVAS
        (
         (modules #:mutable)
         (module_width #:mutable)
         (points_map #:mutable)
         (color_map #:mutable)
         (foreground_color #:mutable)
         (background_color #:mutable)
         )
        #:transparent
        )

(define (fill-color canvas point_list color)
  (for-each
   (lambda (point)
     (hash-set! (CANVAS-points_map canvas) point color))
   point_list))
