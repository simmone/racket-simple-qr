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

