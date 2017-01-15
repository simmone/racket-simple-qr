#lang racket

(require racket/draw)

(let* ([bitmap (make-object bitmap% 700 700)]
       [dc (send bitmap make-dc)])
  (send dc translate 350 350)
  (send dc rotate (* pi 2))
  (send dc draw-bitmap 
        (make-object bitmap% "real.jpg" 'jpeg)
        0 0)
  
  (send bitmap save-file "rotated.png" 'png))


      
