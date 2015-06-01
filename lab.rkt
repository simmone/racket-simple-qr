#lang racket

(require racket/draw)

(let* ([matrix 5]
       [brick_width 10]
       [canvas_width (* (+ matrix 2) brck_width)]
       )

  (define target (make-bitmap canvas_width canvas_width))

  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-smoothing 'smoothed)
  (white-brush dc 0 0 canvas_width canvas_width)

  (send target save-file "box.png" 'png)

  (system "open box.png")
)

(define (white-brush dc x y width height)
  (send dc set-pen "white" 1 'solid)
  (send dc set-brush "white" 'solid)

  (send dc draw-rectangle x y width height))

(define (black-brush dc x y width height)
  (send dc set-pen "black" 1 'solid)
  (send dc set-brush "black" 'solid)

  (send dc draw-rectangle x y width height))

(define (locate-brick matrix row col)
  )

  
  
