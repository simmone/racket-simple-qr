#lang racket

(require racket/draw)

(provide (contract-out
          [express (-> boolean? procedure? void?)]
          [get-points-between (-> pair? pair? #:direction (or/c 'horizontal 'vertical) list?)]
          [get-points (-> (listof list?) (listof pair?) any)]
          [get-unmask-points (-> (listof list?) (listof pair?) procedure? pair?)]
          [bitmap->points (-> (is-a?/c bitmap%) (listof list?))]
          [pic->points (-> path-string? (listof list?))]
          [points->pixels (-> (listof list?) hash? bytes?)]
          [points->points_map (-> (listof list?) hash?)]
          [points->base1_points (-> list? list?)]
          [move-point-col (-> pair? exact-integer? pair?)]
          [move-point-row (-> pair? exact-integer? pair?)]
          [display-list (->* (list?) (natural? natural?) string?)]
          [display-double-list (->* (list? list?) (natural? natural?) string?)]
          [format-string (-> string? natural? string?)]
          [display-qr-bits (-> natural? hash? string?)]
          [split-string (-> string? natural? list?)]
          [locate-finder-pattern (-> natural? list?)]
          ))

(define (move-point-col point cols)
  (cons
   (car point)
   (+ (cdr point) cols)))

(define (move-point-row point rows)
  (cons
   (+ (car point) rows)
   (cdr point)))

(define (locate-finder-pattern modules)
  (list
   '(1 . 1)
   (cons 1 (add1 (- modules 7)))
   (cons (add1 (- modules 7)) 1)))

(define (express express? proc)
  (when express?
        (proc)))

(define (display-list input_list [col_width 12] [line_count 10])
  (with-output-to-string
    (lambda ()
      (printf "@verbatim{\n")
      (let loop ([loop_list input_list]
                 [print_count 0]
                 [item_number 1]
                 [line_start? #t]
                 [origin? #t])
        (when (not (null? loop_list))
              (when line_start?
                    (printf (~a #:min-width 6 #:align 'left #:right-pad-string " " (number->string item_number))))

              (if (or (= print_count (sub1 line_count)) (= (length loop_list) 1))
                  (begin
                    (printf (~a #:min-width col_width #:align 'left #:right-pad-string " " (format "~a" (car loop_list))))
                    (printf "\n")
                    (loop (cdr loop_list) 0 (add1 item_number) #t #f))
                  (begin
                    (printf (~a #:min-width col_width #:align 'left #:right-pad-string " " (format "~a" (car loop_list))))
                    (loop (cdr loop_list) (add1 print_count) (add1 item_number) #f #t)))))
      (printf "}"))))

(define (display-double-list input_list result_list [col_width 12] [line_count 10])
  (if (and
       (not (null? input_list))
       (= (length input_list) (length result_list)))
      (with-output-to-string
        (lambda ()
          (printf "@verbatim{\n")
          (let loop ([loop_list result_list]
                     [origin_list input_list]
                     [print_count 0]
                     [item_number 1]
                     [line_start? #t]
                     [origin? #t])
            (when (not (null? loop_list))
                  (if origin?
                      (begin
                        (when line_start?
                              (printf (~a #:min-width 6 #:align 'left #:right-pad-string " ")))
                        
                        (if (or (= print_count (sub1 line_count)) (= (length origin_list) 1))
                            (begin
                              (printf (~a #:min-width col_width #:align 'left #:right-pad-string " " (format "[~a]" (car origin_list))))
                              (printf "\n")
                              (loop loop_list (cdr origin_list) 0 item_number #t #f))
                            (begin
                              (printf (~a #:min-width col_width #:align 'left #:right-pad-string " " (format "[~a]" (car origin_list))))
                              (loop loop_list (cdr origin_list) (add1 print_count) item_number #f #t))))
                      (begin
                        (when line_start?
                              (printf (~a #:min-width 6 #:align 'left #:right-pad-string " " (number->string item_number))))
                        
                        (if (or (= print_count (sub1 line_count)) (= (length loop_list) 1))
                            (begin
                              (printf (~a #:min-width col_width #:align 'left #:right-pad-string " " (format "~a" (car loop_list))))
                              (printf "\n")
                              (loop (cdr loop_list) origin_list 0 (add1 item_number) #t #t))
                            (begin
                              (printf (~a #:min-width col_width #:align 'left #:right-pad-string " " (format "~a" (car loop_list))))
                              (loop (cdr loop_list) origin_list (add1 print_count) (add1 item_number) #f #f)))))))
          (printf "}")))
      ""))

(define (display-qr-bits modules points_map)
  (with-output-to-string
    (lambda ()
      (printf "@verbatim{\n")
      
      (printf "   ")

      (let loop ([col 1])
        (when (<= col modules)
              (printf (~a #:min-width 3 #:align 'right #:left-pad-string " " col))
              (loop (add1 col))))
      
      (let loop ([row 1]
                 [col 1])

        (when (and (<= row modules) (<= col modules))
              (when (= col 1)
                    (printf "\n")
                    (printf (~a #:min-width 3 #:align 'right #:left-pad-string " " row)))

              (printf (~a #:min-width 3 #:align 'right #:left-pad-string " " 
                          (hash-ref points_map (cons row col) "*")))

              (if (= col modules)
                  (loop (add1 row) 1)
                  (loop row (add1 col)))))

      (printf "\n   ")

      (let loop ([col 1])
        (when (<= col modules)
              (printf (~a #:min-width 3 #:align 'right #:left-pad-string " " col))
              (loop (add1 col))))

      (printf "}"))))

(define (points->pixels points_list pixel_map)
  (let loop ([rows points_list]
             [row_index 0]
             [bytes_list '()])
    (if (not (null? rows))
        (loop
         (cdr rows)
         (add1 row_index)
         (cons
          (let col-loop ([cols (car rows)]
                         [col_index 0]
                         [col_bytes_list '()])
            (if (not (null? cols))
                (if (hash-has-key? pixel_map (cons row_index col_index))
                    (col-loop (cdr cols) (add1 col_index) `(,@(hash-ref pixel_map (cons row_index col_index)) ,@col_bytes_list))
                    (if (= (car cols) 0)
                        (col-loop (cdr cols) (add1 col_index) (cons 255 (cons 255 (cons 255 (cons 255 col_bytes_list)))))
                        (col-loop (cdr cols) (add1 col_index) (cons 0 (cons 0 (cons 0 (cons 255 col_bytes_list)))))))
                (reverse col_bytes_list)))
          bytes_list))
        (list->bytes (foldr (lambda (a b) (append a b)) '() (reverse bytes_list))))))

(define (points->points_map points_list)
  (let ([points_map (make-hash)])
    (let row-loop ([row_list points_list]
                   [row_count 1])
      (when (not (null? row_list))
            (let col-loop ([col_list (car row_list)]
                           [col_count 1])
              (when (not (null? col_list))
                    (hash-set! points_map (cons row_count col_count) (car col_list))
                    (col-loop (cdr col_list) (add1 col_count))))
            (row-loop (cdr row_list) (add1 row_count))))
    points_map))

(define(points->base1_points points)
  (map
   (lambda (point)
     (cons (add1 (car point)) (add1 (cdr point))))
   points))

(define (bitmap->points img)
  (let* ([width (send img get-width)]
         [height (send img get-height)]
         [bits_count (* width height 4)])

    (let ([bits_bytes (make-bytes bits_count)])
      (send img get-argb-pixels 0 0 width height bits_bytes)
      
      (let loop ([loop_list (bytes->list bits_bytes)]
                 [rows '()]
                 [cols '()])
        (if (= (length rows) height)
            (reverse rows)
            (if (= (length cols) width)
                (loop loop_list (cons (reverse cols) rows) '())
                (loop (cdr (cdr (cdr (cdr loop_list)))) 
                      rows
                      (cons (+ (list-ref loop_list 1) (list-ref loop_list 2) (list-ref loop_list 3)) cols))))))))

(define (pic->points pic_path)
  (bitmap->points (make-object bitmap% pic_path)))

(define (get-points matrix trace_list)
  (let loop ([loop_list trace_list]
             [result_list '()])
    (if (not (null? loop_list))
        (let* ([i (sub1 (caar loop_list))]
               [j (sub1 (cdar loop_list))]
               [val (list-ref (list-ref matrix i) j)])
          (loop (cdr loop_list) (cons val result_list)))
        (reverse result_list))))

(define (get-unmask-points matrix trace_list mask-proc)
  (let loop ([loop_list trace_list]
             [result_list '()]
             [mask_list '()])
    (if (not (null? loop_list))
        (let* ([i (sub1 (caar loop_list))]
               [j (sub1 (cdar loop_list))]
               [val (list-ref (list-ref matrix i) j)]
               [mask (if (mask-proc i j) 1 0)])
          (loop (cdr loop_list) (cons (bitwise-xor val mask) result_list) (cons mask mask_list)))
        (cons (reverse result_list) (reverse mask_list)))))

(define (get-points-between start_point end_point #:direction direction)
  (let ([is_valid? #f])
    (if (equal? direction 'horizontal)
        (when (and
               (= (car start_point) (car end_point))
               (<= (cdr start_point) (cdr end_point)))
              (set! is_valid? #t))
        (when (and
               (= (cdr start_point) (cdr end_point))
               (<= (car start_point) (car end_point)))
              (set! is_valid? #t)))
    (if (not is_valid?)
        '()
        (let ([points '()]
              [vars '()]
              [start_var (if (equal? direction 'horizontal) (cdr start_point) (car start_point))]
              [end_var (if (equal? direction 'horizontal) (cdr end_point) (car end_point))])
          (let loop ([var start_var])
            (when (<= var end_var)
                  (set! vars `(,@vars ,var))
                  (loop (add1 var))))
          (for-each
           (lambda (cor)
             (if (equal? direction 'horizontal)
                 (set! points `(,@points ,(cons (car start_point) cor)))
                 (set! points `(,@points ,(cons cor (cdr start_point))))))
           vars)
          points))))

(define (split-string bit_str width)
  (let loop ([loop_str bit_str]
             [result_list '()])
    (if (not (string=? loop_str ""))
        (if (>= (string-length loop_str) 8)
            (loop (substring loop_str width) (cons (substring loop_str 0 width) result_list))
            (loop "" (cons loop_str result_list)))
        (reverse result_list))))

(define (format-string data line_count)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_str data]
                 [result_str ""])
        (if (> (string-length loop_str) line_count)
            (loop (substring loop_str line_count) (printf "~a\n" (substring loop_str 0 line_count)))
            (printf "~a\n" loop_str))))))
