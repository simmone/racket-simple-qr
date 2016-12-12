#lang racket

(provide (contract-out
          [qr-read (-> path-string? string?)]
          ))

(require racket/draw)

(define (qr-read pic_path)
  (let* ([img (make-object bitmap% "test.png")]
         [width (send img get-width)]
         [height (send img get-height)]
         [bits_count (* width height 32)])

    (let ([bits_bytes (make-bytes bits_count)])
      (send img get-argb-pixels 0 0 width height bits_bytes)
      
      (printf "~a\n" (first (bytes->list bits_bytes)))
      )
    ))

(qr-read "test.png")
