#lang racket

(require racket/gui)
(require racket/draw)

;(define (display-bitmap bitmap)
;  (define f
;    (new frame%
;         [label "Bitmap"]
;         [width (send bitmap get-width)]
;         [height (send bitmap get-height)]))
;  (define c (new canvas%
;                 [parent f]
;                 [paint-callback
;                  (lambda (canvas dc)
;                    (send dc translate 10 10)
;                    (send dc rotate 0.1)
;                    (send dc draw-bitmap bitmap 0 0))]))
;
;  (send f show #t)
;  f)
;
;(define texture  (make-object bitmap% "step3_bw.png" 'png))
;
;(display-bitmap texture)

(let* ([bitmap (make-object bitmap% 400 400)]
       [dc (send bitmap make-dc)])
  (send dc translate 200 200)
  (send dc rotate 6.5)
  (send dc draw-bitmap 
        (make-object bitmap% "step3_bw.png" 'png)
        0 0)
  
  (send bitmap save-file "rotated.png" 'png))


      
