#lang racket

(require racket/draw)

(require "func.rkt")

(let* ([version 21]
       [module_width 20]
       [canvas_width (* version module_width)]
       )

  (define target (make-bitmap canvas_width canvas_width))
  
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-smoothing 'smoothed)

  (white-block dc '(0 . 0) canvas_width)

  (draw-finder-pattern dc version module_width)

  (send target save-file "box.png" 'png)

  (system "open box.png")
)
