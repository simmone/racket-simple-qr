#lang racket

(require "func.rkt")

(let* ([matrix 5]
       [brick_width 10]
       [canvas_width (* (+ matrix 2) brick_width)]
       )

  (define target (make-bitmap canvas_width canvas_width))
  
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-smoothing 'smoothed)
  (white-brush dc 0 0 canvas_width canvas_width)

  (send target save-file "box.png" 'png)

  (system "open box.png")
)
