#lang racket

(require racket/draw)

(let* ([bitmap (make-object bitmap% 700 700)]
       [dc (send bitmap make-dc)])
  (send dc translate 350 350)
  (send dc rotate (* (* pi 2) -0.1))
  (send dc draw-bitmap 
        (make-object bitmap% "step3_bw.png" 'png)
        0 0)
  
  (send bitmap save-file "rotated.png" 'png))


      
