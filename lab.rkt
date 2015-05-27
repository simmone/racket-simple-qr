#lang racket

(require racket/draw)

(define target (make-bitmap 100 100))

(define dc (new bitmap-dc% [bitmap target]))

(send dc set-pen "black" 4 'solid)

(send dc set-smoothing 'smoothed)

(send dc draw-rectangle 0 0 100 100)

(send dc set-pen "red" 1 'solid)

(send dc draw-line 0 0 30 30)

(send dc draw-line 0 30 30 0)

(send target save-file "box.png" 'png)

(system "open box.png")
