#lang racket

(provide (contract-out
          [qr-read (-> path-string? string?)]
          ))

(require racket/draw)

(define (qr-read pic_path)
  (let* ([img (make-object bitmap% "test.png")]
         [width (send img get-width)]
         [height (send img get-height)]
         [bits_count (* width height 4)]
         [points_list #f])

    (let ([bits_bytes (make-bytes bits_count)])
      (send img get-argb-pixels 0 0 width height bits_bytes)
      
      (set! points_list
            (let loop ([loop_list bits_bytes]
                       [rows '()]
                       [cols '()])
              (if (= (length rows) height)
                  (reverse rows)
                  (if (= (length cols) width)
                        (loop loop_list (cons (reverse cols) rows) '())
                        (loop (cdr (cdr (cdr (cdr loop_list)))) 
                              rows
                              (cons (+ (list-ref loop_list 1) (list-ref loop_list 2) (list-ref loop_list 3)))))))))
      )
    ))

(qr-read "test.png")
