#lang racket

(provide (contract-out
          [pic->points (-> path-string? list?)]
          [points->pic (-> (listof list?) path-string? any)]
          [find-threshold (-> list? exact-nonnegative-integer?)]
          [points->bw (-> list? exact-nonnegative-integer? list?)]
          [print-points (-> list? void?)]
          [guess-first-dark-width (-> list? exact-nonnegative-integer?)]
          [guess-module-width (-> list? (or/c boolean? exact-nonnegative-integer?))]
          [squash-points (-> list? exact-nonnegative-integer? list?)]
          [*trace_level* parameter?]
          [trace (-> string? exact-nonnegative-integer? void?)]
          [qr-read (-> path-string? (or/c string? boolean?))]
          [verify-matrix (-> (listof list?) (or/c (listof list?) boolean?))]
          [squash-matrix (-> (listof list?) exact-nonnegative-integer? (listof list?))]
          [try-to-get-matrix (-> (listof list?) 
                                 exact-nonnegative-integer? 
                                 exact-nonnegative-integer? 
                                 exact-nonnegative-integer? 
                                 (or/c (listof list?) boolean?))]
          ))

(require racket/draw)
(require matrix-rotate)

(define *trace_level* (make-parameter 0))

(define (trace data trace_level)
  (when (>= (*trace_level*) trace_level)
        (printf "t[~a]=~a\n" trace_level data)))

(define (pic->points pic_path)
  (let* ([img (make-object bitmap% pic_path)]
         [width (send img get-width)]
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

(define (find-threshold points_list)
  (let ([max_value 0]
        [min_value 765])
    (let row-loop ([loop_row_list points_list])
      (if (not (null? loop_row_list))
          (begin
            (let col-loop ([loop_col_list (car loop_row_list)])
              (when (not (null? loop_col_list))
                    (cond
                     [(> (car loop_col_list) max_value)
                      (set! max_value (car loop_col_list))]
                     [(< (car loop_col_list) min_value)
                      (set! min_value (car loop_col_list))]
                     )
                    (col-loop (cdr loop_col_list))))
            (row-loop (cdr loop_row_list)))
          (floor (/ (- max_value min_value) 2))))))

(define (points->bw points_list threshold)
  (map
   (lambda (row)
     (map
      (lambda (col)
        (if (> col threshold) 0 1))
      row))
   points_list))

(define (print-points points_list)
  (let row-loop ([loop_row_list points_list])
    (when (not (null? loop_row_list))
        (let col-loop ([loop_col_list (car loop_row_list)])
          (if (not (null? loop_col_list))
              (begin
                (printf "~a" (car loop_col_list))
                (col-loop (cdr loop_col_list)))
              (printf "\n")))
        (row-loop (cdr loop_row_list)))))

(define (points->pic points_list pic_path)
  (let* ([width (length (car points_list))]
         [height (length points_list)]
         [points_pic (make-object bitmap% width height)])
    (send points_pic set-argb-pixels 0 0 width height 
          (let loop ([rows points_list]
                     [bytes_list '()])
            (if (not (null? rows))
                (loop
                 (cdr rows)
                 (cons
                  (let col-loop ([cols (car rows)]
                                 [col_bytes_list '()])
                    (if (not (null? cols))
                        (if (= (car cols) 0)
                            (col-loop (cdr cols) (cons 255 (cons 255 (cons 255 (cons 255 col_bytes_list)))))
                            (col-loop (cdr cols) (cons 0 (cons 0 (cons 0 (cons 255 col_bytes_list))))))
                        (reverse col_bytes_list)))
                  bytes_list))
                (list->bytes (foldr (lambda (a b) (append a b)) '() (reverse bytes_list))))))
    (send points_pic save-file pic_path 'png)))

(define (guess-first-dark-width points)
  (let loop ([points_loop points]
             [dark_length 0])
    (if (not (null? points_loop))
        (if (= (car points_loop) 0)
            (if (> dark_length 0)
                dark_length
                (loop (cdr points_loop) dark_length))
            (loop (cdr points_loop) (add1 dark_length)))
        dark_length)))

(define (squash-points points width)
  (let ([min_width (floor (* width 0.5))])
    (let loop ([loop_points points]
               [last_value -1]
               [same_count 0]
               [result_list '()])

      (if (not (null? loop_points))
          (if (= same_count width)
              (loop (cdr loop_points) (car loop_points) 1 (cons last_value result_list))
              (if (= (car loop_points) last_value)
                  (loop (cdr loop_points) last_value (add1 same_count) result_list)
                  (if (= last_value -1)
                      (loop (cdr loop_points) (car loop_points) (add1 same_count) result_list)
                      (if (>= same_count min_width)
                          (loop (cdr loop_points) (car loop_points) 1 (cons last_value result_list))
                          (loop (cdr loop_points) (car loop_points) 1 result_list)))))
          (if (and (> same_count 0) (>= same_count min_width))
              (reverse (cons last_value result_list))
              (reverse result_list))))))

(define (guess-module-width points_row)
  (let ([max_module_width (floor (/ (length points_row) 14))])
    (let loop ([points points_row])
      (if (not (null? points))
          (if (= (car points) 1)
              (let* ([guess_module_width (guess-first-dark-width points)]
                     [squashed_line (squash-points points guess_module_width)]
                     [squashed_str 
                      (foldr (lambda (a b) (string-append a b)) "" (map (lambda (b) (number->string b)) squashed_line))])
                (if (= (length (regexp-match* #rx"1011101" squashed_str)) 2)
                    guess_module_width
                    (loop (list-tail points guess_module_width))))
              (loop (cdr points)))
          #f))))

(define (squash-matrix matrix module_width)
  (map
   (lambda (row)
     (squash-points row module_width))
   matrix))

(define (carve-matrix matrix left_up_point right_down_point)
  (let ([start_x (car left_up_point)]
        [start_y (cdr left_up_point)]
        [end_x (car right_down_point)]
        [end_y (cdr right_down_point)])
  (let loop ([loop_index start_x]
             [result_list '()])
    (if (<= loop_index end_x)
        (loop (add1 loop_index)
              (cons
               (take (drop (list-ref matrix loop_index) start_y) (add1 (- end_y start_y)))
               result_list))
        (reverse result_list)))))

(define (verify-matrix matrix)
  (let* ([matrix_width (length matrix)]
         [expected_matrix '(
                            (1 1 1 1 1 1 1)
                            (1 0 0 0 0 0 1)
                            (1 0 1 1 1 0 1)
                            (1 0 1 1 1 0 1)
                            (1 0 1 1 1 0 1)
                            (1 0 0 0 0 0 1)
                            (1 1 1 1 1 1 1)
                            )]
         [finder_up_left (carve-matrix matrix '(0 . 0) '(6 . 6))]
         [finder_up_right (carve-matrix matrix (cons 0 (- matrix_width 7)) (cons 6 (sub1 matrix_width)))]
         [finder_down_left (carve-matrix matrix (cons (- matrix_width 7) 0) (cons (sub1 matrix_width) 6))])
    (if (and
         (equal? finder_up_left expected_matrix)
         (equal? finder_up_right expected_matrix)
         (equal? finder_down_left expected_matrix))
        matrix
        #f)))

(define (try-to-get-matrix matrix row_index finder_pattern1_index finder_pattern2_index)
  (let* ([matrix_width (length matrix)]
         [matrix_height (length (car matrix))]
         [guess_matrix_width (+ (- finder_pattern2_index finder_pattern1_index) 7)]
         [left_up_point (cons (- row_index 2) finder_pattern1_index)]
         [right_down_point (cons (sub1 (+ (- row_index 2) guess_matrix_width)) (+ finder_pattern2_index 6))])
    (if (and (>= (car left_up_point) 0) (<= (cdr right_down_point) (sub1 matrix_width)))
        (verify-matrix (carve-matrix matrix left_up_point right_down_point))
        #f)))

(define (qr-read pic_path)
  (let* ([step1_points_list #f]
         [original_height (length step1_points_list)]
         [original_width (length (car step1_points_list))]
         [rotate_max_tries (- (+ (* original_width 2) (* original_height 2)) 4)]
         [step2_threshold #f]
         [step3_bw_points #f]
         [step4_qr_points #f])

    (set! step1_points_list (pic->points pic_path))
    (trace (format "step1:convert pic file to pixel points[~aX~a]" original_width original_height) 1)

    (set! step2_threshold (find-threshold step1_points_list))
    (trace (format "step2:find threshold is ~a" step2_threshold) 1)

    (set! step3_bw_points (points->bw step1_points_list step2_threshold))
    (trace (format "step3:use threshold convert pixel to points 0 or 1") 1)

    (points->pic step3_bw_points "step3_bw.png")

    (set! step4_qr_points
          (let rotate-loop ([tries 0]
                            [matrix step3_bw_points])
            (if (< tries rotate_max_tries)
                (let ([verified_matrix (verify-matrix matrix)])
                  (if verified_matrix
                      verified_matrix
                      (rotate-loop (add1 tries) (matrix-rotate matrix (add1 tries)))))
                #f)))

    (if (not step4_qr_points)
        (points->pic step4_qr_points "qr.png")
        "")))
