#lang racket

(require racket/draw
         file/sha1)

(provide (contract-out
          [transform-points-list (-> list? pair? list?)]
          [locate-brick (-> natural? pair? pair?)]
          [hex_color->racket_color (-> string? (or/c string? (is-a?/c color%)))]
          [get-points-between (-> pair? pair? #:direction (or/c 'horizontal 'vertical 'cross) list?)]
          [split-string (-> string? natural? list?)]
          [string-to-bits-markdown-table (-> string? string? string?)]
          [bits-to-markdown-table (-> string? natural? string?)]

          [get-points (-> (listof list?) (listof pair?) any)]
          [get-unmask-points (-> (listof list?) (listof pair?) procedure? pair?)]
          [bitmap->points (-> (is-a?/c bitmap%) (listof list?))]
          [pic->points (-> path-string? (listof list?))]
          [points->pixels (-> (listof list?) hash? bytes?)]
          [points->points_map (-> (listof list?) hash?)]
          [points->base1_points (-> list? list?)]
          [move-point-col (-> pair? exact-integer? pair?)]
          [move-point-row (-> pair? exact-integer? pair?)]
          [format-string (-> string? natural? string?)]
          [display-qr-bits (-> natural? hash? string?)]
          ))

(define (transform-points-list points_list start_point_pair)
  (map
   (lambda (point)
     (cons (+ (car start_point_pair) (car point)) (+ (cdr start_point_pair) (cdr point))))
   points_list))

(define (locate-brick module_width place_pair)
  (cons (* (cdr place_pair) module_width)
        (* (car place_pair) module_width)))

(define (hex_color->racket_color hex_color)
  (if (regexp-match #px"^#([0-9a-zA-Z]{6})$" hex_color)
      (let ([rgb_list (bytes->list (hex-string->bytes (substring hex_color 1)))])
        (make-object color% (first rgb_list) (second rgb_list) (third rgb_list)))
      hex_color))

(define (get-points-between start_point end_point #:direction direction)
  (let ([is_valid?
         (cond
          [(and
            (eq? direction 'horizontal)
            (= (car start_point) (car end_point))
            (<= (cdr start_point) (cdr end_point)))
           #t]
          [(and
            (eq? direction 'vertical)
            (<= (car start_point) (car end_point))
            (= (cdr start_point) (cdr end_point)))
           #t]
          [(and
            (eq? direction 'cross)
            (<= (car start_point) (car end_point))
            (<= (cdr start_point) (cdr end_point)))
           #t]
          [else
           #f])])
        
    (if (not is_valid?)
        '()
        (let loop-x ([loop_x (car start_point)]
                     [result_points '()])
          (if (<= loop_x (car end_point))
              (loop-x (add1 loop_x)
                      (append
                       result_points
                       (let loop-y ([loop_y (cdr start_point)]
                                    [y_result '()])
                         (if (<= loop_y (cdr end_point))
                             (loop-y (add1 loop_y) (cons (cons loop_x loop_y) y_result))
                             (reverse y_result)))))
              result_points)))))

(define (split-string bit_str width)
  (let loop ([loop_str bit_str]
             [result_list '()])
    (if (not (string=? loop_str ""))
        (if (>= (string-length loop_str) width)
            (loop (substring loop_str width) (cons (substring loop_str 0 width) result_list))
            (loop "" (cons loop_str result_list)))
        (reverse result_list))))

(define (string-to-bits-markdown-table data bits)
  (with-output-to-string
    (lambda ()
      (printf "|char|byte|\n|---|---|\n")
      (let loop ([chars (string->list data)]
                 [bytes (split-string bits 8)])
        (when (not (null? bytes))
          (printf "|~a|~a|\n" (car chars) (car bytes))
          (loop (cdr chars) (cdr bytes)))))))

(define (bits-to-markdown-table bits line_width)
  (with-output-to-string
    (lambda ()
      (printf "|index|bits|\n|---|---|\n")
      (let loop ([bytes (split-string bits line_width)]
                 [index 1])
        (when (not (null? bytes))
          (printf "|~a|~a|\n" index (car bytes))
          (loop (cdr bytes) (add1 index)))))))

(define (move-point-col point cols)
  (cons
   (car point)
   (+ (cdr point) cols)))

(define (move-point-row point rows)
  (cons
   (+ (car point) rows)
   (cdr point)))

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

(define (format-string data line_count)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_str data]
                 [result_str ""])
        (if (> (string-length loop_str) line_count)
            (loop (substring loop_str line_count) (printf "~a\n" (substring loop_str 0 line_count)))
            (printf "~a\n" loop_str))))))
