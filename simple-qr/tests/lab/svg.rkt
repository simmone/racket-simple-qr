#lang racket

(require racket/draw)

(let ([dc (new svg-dc% [width 100] [height 100] [output "test.svg"] [exists 'replace])])
    
  (send dc start-doc "start")
  
  (send dc start-page)
  
  (send dc set-pen "red" 1 'solid)

  (send dc set-brush "red" 'solid)

  (send dc draw-rectangle 25 25 50 50)
  
  (send dc end-page)
  
  (send dc end-doc))
