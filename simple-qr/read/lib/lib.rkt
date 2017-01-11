#lang racket

(provide (contract-out
          [pic->points (-> path-string? list?)]
          [points->pic (-> (listof list?) path-string? hash? any)]
          [find-threshold (-> list? exact-nonnegative-integer?)]
          [points->bw (-> list? exact-nonnegative-integer? list?)]
          [print-points (-> list? void?)]
          [guess-first-dark-width (-> list? exact-nonnegative-integer?)]
          [guess-module-width (-> (or/c #f exact-nonnegative-integer?) list? (or/c boolean? list?))]
          [squash-points (-> list? exact-nonnegative-integer? list?)]
          [*trace_level* parameter?]
          [trace (-> string? exact-nonnegative-integer? void?)]
          [qr-read (-> path-string? (or/c string? boolean?))]
          [squash-matrix (-> (listof list?) exact-nonnegative-integer? (listof list?))]
          [point-distance (-> pair? pair? number?)]
          [find-pattern-center-points (-> (listof list?) (or/c boolean? list?))]
          [check-center-points-valid (-> hash? boolean?)]
          [get-center-points (-> hash? list?)]
          [calculate-rotate-ratio (-> pair? pair? exact-nonnegative-integer? number?)]
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

(define (points->pic points_list pic_path pixel_map)
  (let* ([width (length (car points_list))]
         [height (length points_list)]
         [points_pic (make-object bitmap% width height)])
    (send points_pic set-argb-pixels 0 0 width height 
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

(define (guess-module-width guess_module_width points_row)
  (let ([max_module_width (floor (/ (length points_row) 14))])
    (let loop ([points points_row])
      (if (not (null? points))
          (if (= (car points) 1)
              (begin
                (when (not guess_module_width)
                      (set! guess_module_width (guess-first-dark-width points)))

                (let* ([squashed_line (squash-points points_row guess_module_width)]
                       [squashed_str 
                        (foldr (lambda (a b) (string-append a b)) "" (map (lambda (b) (number->string b)) squashed_line))])

                  (if (regexp-match #rx"010111010" squashed_str)
                      (begin
                        (trace (format "~a" squashed_str) 1)
                        (cons
                         guess_module_width
                         (map
                          (lambda (item)
                            (* guess_module_width (add1 (car item))))
                          (regexp-match-positions* #rx"010111010" squashed_str))))
                      (if (> (length points) guess_module_width)
                          (loop (list-tail points guess_module_width))
                          #f))))
              (loop (cdr points)))
          #f))))

(define (guess-matrix matrix)
  (let loop ([rows matrix]
             [row_index 0]
             [result_list '()]
             [guess_module_width #f])
    (if (not (null? rows))
        (let ([guess_result (guess-module-width guess_module_width (car rows))])
          (when guess_result 
                (trace (format "row:~a" row_index) 1)
                (trace (format "guess_result:~a" guess_result) 1))

          (if guess_result
              (loop 
               (cdr rows) 
               (add1 row_index) 
               (cons `(,row_index ,@(cdr guess_result)) result_list)
               (car guess_result))
              (loop 
               (cdr rows) 
               (add1 row_index) 
               result_list
               guess_module_width)))
        (cons guess_module_width (reverse result_list)))))

(define (check-matrix-integrity matrix)
  (let ([width (length (car matrix))])
    (andmap (lambda (item) (= (length item) width)) matrix)))

(define (squash-matrix matrix module_width)
  (let ([squash_matrix_x
         (map
          (lambda (row)
            (squash-points row module_width))
          matrix)])
    (if (check-matrix-integrity squash_matrix_x)
        (let* ([rotate_matrix (matrix-row->col squash_matrix_x)]
               [squash_matrix_y
                (map
                 (lambda (row)
                   (squash-points row module_width))
                 rotate_matrix)])
          (if (check-matrix-integrity squash_matrix_y)
              (matrix-col->row squash_matrix_y)
              #f))
        #f)))

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

(define (find-pattern-center guess_results)
  (let ([group_map (make-hash)])
    (let loop ([guesses guess_results]
               [group_start_x -1]
               [group_end_x -1])
      (when (not (null? guesses))
            (let ([guess_result (car guesses)])
              (let ([point_x (first guess_result)]
                    [point_y_list (cdr guess_result)])

                (if (not (= point_x group_end_x))
                    (begin
                      (set! group_start_x point_x)
                      (set! group_end_x (add1 point_x)))
                    (set! group_end_x (add1 group_end_x)))
                
                (for-each
                 (lambda (point_y)
                   (let ([start_point (cons group_start_x point_y)])
                     (if (hash-has-key? group_map start_point)
                         (hash-set! group_map start_point `(,@(hash-ref group_map start_point) ,(cons point_x point_y)))
                         (hash-set! group_map start_point `(,(cons point_x point_y))))))
                 point_y_list)))
            (loop (cdr guesses) group_start_x group_end_x)))
    group_map))

(define (check-center-points-valid points_distance_map)
  (if (not (= (hash-count points_distance_map) 6))
      #f
      (if (not 
           (andmap
            (lambda (item)
              (let* ([point_pair (car item)]
                     [distance (cdr item)]
                     [point_a (car point_pair)]
                     [point_b (cdr point_pair)]
                     [opposite_pair (cons point_b point_a)])
                (and
                 (hash-has-key? points_distance_map opposite_pair)
                 (= distance (hash-ref points_distance_map opposite_pair)))))
            (hash->list points_distance_map)))
          #f
          #t)))

(define (get-center-points points_distance_map)
  (let ([points_hash (make-hash)]
        [max_distance 0]
        [point_a #f]
        [point_maybe_b #f]
        [point_maybe_c #f]
        [point_b #f]
        [point_c #f])

    (hash-for-each
     points_distance_map
     (lambda (points_pair distance)
       (when (> distance max_distance)
             (set! max_distance distance))
       (hash-set! points_hash (car points_pair) "")))

    (let ([center_points (hash-keys points_hash)])
      (if (= (hash-ref points_distance_map (cons (list-ref center_points 0) (list-ref center_points 1))) max_distance)
          (begin
            (set! point_a (str->point (list-ref center_points 2)))
            (set! point_maybe_b (str->point (list-ref center_points 0)))
            (set! point_maybe_c (str->point (list-ref center_points 1))))
          (if (= (hash-ref points_distance_map (cons (list-ref center_points 0) (list-ref center_points 2))) max_distance)
              (begin
                (set! point_a (str->point (list-ref center_points 1)))
                (set! point_maybe_b (str->point (list-ref center_points 0)))
                (set! point_maybe_c (str->point (list-ref center_points 2))))
              (begin
                (set! point_a (str->point (list-ref center_points 0)))
                (set! point_maybe_b (str->point (list-ref center_points 1)))
                (set! point_maybe_c (str->point (list-ref center_points 2)))))))

    (let ([point_a_x (car point_a)]
          [point_a_y (cdr point_a)])
      (cond
       [(and
         (>= (cdr point_maybe_b) point_a_y)
         (>= (cdr point_maybe_c) point_a_y))
        (if (< (car point_maybe_b) (car point_maybe_c))
            (begin
              (set! point_b point_maybe_b)
              (set! point_c point_maybe_c))
            (begin
              (set! point_b point_maybe_c)
              (set! point_c point_maybe_b)))]
       [(and
         (<= (cdr point_maybe_b) point_a_y)
         (<= (cdr point_maybe_c) point_a_y))
        (if (> (car point_maybe_b) (car point_maybe_c))
            (begin
              (set! point_b point_maybe_b)
              (set! point_c point_maybe_c))
            (begin
              (set! point_b point_maybe_c)
              (set! point_c point_maybe_b)))]
       [(and
         (>= (car point_maybe_b) point_a_x)
         (>= (car point_maybe_c) point_a_x))
        (if (> (cdr point_maybe_b) (cdr point_maybe_c))
            (begin
              (set! point_b point_maybe_b)
              (set! point_c point_maybe_c))
            (begin
              (set! point_b point_maybe_c)
              (set! point_c point_maybe_b)))]
       [(and
         (<= (car point_maybe_b) point_a_x)
         (<= (car point_maybe_c) point_a_x))
        (if (< (cdr point_maybe_b) (cdr point_maybe_c))
            (begin
              (set! point_b point_maybe_b)
              (set! point_c point_maybe_c))
            (begin
              (set! point_b point_maybe_c)
              (set! point_c point_maybe_b)))]))

      (list point_a point_b point_c)))

(define (point-distance point_x point_y)
  (ceiling
   (sqrt (+ 
          (expt (- (car point_x) (car point_y)) 2)
          (expt (- (cdr point_x) (cdr point_y)) 2)))))

(define (point->str point)
  (string-append (number->string (car point)) "-" (number->string (cdr point))))

(define (str->point str)
  (let ([items (regexp-split #rx"-" str)])
    (cons (string->number (first items)) (string->number (second items)))))

(define (find-pattern-center-points matrix)
  (let* ([guess_results (guess-matrix matrix)]
         [module_width (car guess_results)]
         [group_map (find-pattern-center (cdr guess_results))]
         [group_list #f]
         [all_center_points #f]
         [points_distance_map (make-hash)]
         [center_points #f]
         )
    
    (if (< (hash-count group_map) 3)
        #f
        (begin
          (set! group_list (take (sort (hash-values group_map) (lambda (c d) (> (length c) (length d)))) 3))

          (set! all_center_points
                (map
                 (lambda (group_list)
                   (let* ([center_point (list-ref group_list (floor (/ (length group_list) 2)))]
                          [point_x (car center_point)]
                          [point_y (+ (cdr center_point) (* 3 module_width) (floor (/ module_width 2)))])
                     (cons point_x point_y)))
                 group_list))

          (trace (format "step4 guess_results:~a" guess_results) 1)
          (trace (format "step4 group_map:~a" group_map) 1)
          (trace (format "step4 group_map first 3 group:~a" group_list) 1)
          (trace (format "step4 all_center_points:~a" all_center_points) 1)
    
          (let outer-loop ([points all_center_points])
            (when (not (null? points))
                  (let inner-loop ([inner_points all_center_points])
                    (when (not (null? inner_points))
                          (when (not (equal? (car points) (car inner_points)))
                                (hash-set! points_distance_map 
                                           (cons (point->str (car points)) (point->str (car inner_points)))
                                           (point-distance (car points) (car inner_points))))
                          (inner-loop (cdr inner_points))))
                  (outer-loop (cdr points))))

          (trace (format "step4 points_distance_map:~a" points_distance_map) 1)

          (if (check-center-points-valid points_distance_map)
              (get-center-points points_distance_map)
              #f)))))

(define (calculate-rotate-ratio point_a point_b radius_length)
  0.0)

(define (qr-read pic_path)
  (let* ([step1_points_list #f]
         [original_height #f]
         [original_width #f]
         [step2_threshold #f]
         [step3_bw_points #f]
         [step4_pattern_center_points #f]
         [step5_rotate_ratio #f]
         )

    (set! step1_points_list (pic->points pic_path))
    (set! original_width (length step1_points_list))
    (set! original_height (length (car step1_points_list)))
    (trace (format "step1:convert pic file to pixel points[~aX~a]" original_width original_height) 1)
    
    (set! step2_threshold (find-threshold step1_points_list))
    (trace (format "step2:find threshold is ~a" step2_threshold) 1)

    (set! step3_bw_points (points->bw step1_points_list step2_threshold))
    (trace (format "step3:use threshold convert pixel to points 0 or 1") 1)
    (points->pic step3_bw_points "step3_bw.png" (make-hash))
    
    (set! step4_pattern_center_points (find-pattern-center-points step3_bw_points))
    (trace (format "step4 pattern center points:~a" step4_pattern_center_points) 1)
    (when step4_pattern_center_points
          (let ([pixel_map (make-hash)])
            (hash-set! pixel_map (first step4_pattern_center_points) '(0 0 255 255))
            (hash-set! pixel_map (second step4_pattern_center_points) '(0 255 0 255))
            (hash-set! pixel_map (third step4_pattern_center_points) '(255 0 0 255))

            (points->pic step3_bw_points "step4_pattern_center.png" pixel_map)))
    
    (set! step5_rotate_ratio (calculate-rotate-ratio (first step4_pattern_center_points) (second step4_pattern_center_points)))
    (trace (format "step5 rotate ratio:~a" step5_rotate_ratio) 1)

    ""))
