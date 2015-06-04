#lang racket

(require racket/draw)

(require "func.rkt")

(let* ([version 50]
       [module_width 20]
       [canvas_width (* (+ version 2) module_width)]
       )

  (define target (make-bitmap canvas_width canvas_width))
  
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-smoothing 'smoothed)
  (white-block dc '(0 . 0) (cons canvas_width canvas_width))

  (let loop_row ([row 1])
    (when (<= row version)
          (let loop_col ([col 1])
            (when (<= col version)
                  (when (= (remainder (+ row col) 2) 0)
                        (black-block dc (locate-brick module_width (cons row col)) (cons module_width module_width)))
                  (loop_col (add1 col))))
          (loop_row (add1 row))))

  (send target save-file "box.png" 'png)

  (system "open box.png")
)
