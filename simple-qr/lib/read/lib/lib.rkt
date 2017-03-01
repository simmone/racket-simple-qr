#lang racket

(provide (contract-out
          [find-threshold (-> list? exact-nonnegative-integer?)]
          [points->bw (-> list? exact-nonnegative-integer? list?)]
          [guess-first-dark-width (-> list? exact-nonnegative-integer?)]
          [guess-module-width (-> (or/c #f exact-nonnegative-integer?) list? (or/c boolean? list?))]
          [squash-points (-> list? exact-nonnegative-integer? pair?)]
          [guess_result->pixel_map (-> list? hash?)]
          [qr-read (-> path-string? (or/c string? boolean?))]
          [squash-matrix (-> (listof list?) exact-nonnegative-integer? (listof list?))]
          [point-distance (-> pair? pair? number?)]
          [find-pattern-center-points (-> (listof list?) (or/c boolean? pair?))]
          [check-center-points-valid (-> hash? boolean?)]
          [get-center-points (-> hash? list?)]
          [calculate-rotate-ratio (-> pair? pair? exact-nonnegative-integer? number?)]
          [align-matrix (-> (listof list?) any/c (listof list?))]
          [trim-matrix (-> (listof list?) (listof list?))]
          ))

(require racket/draw)

(require "../matrix-rotate/lib.rkt")
(require "../../share/format-information.rkt")
(require "../../share/finder-pattern.rkt")
(require "../../share/separator.rkt")
(require "../../share/timing-pattern.rkt")
(require "../../share/alignment-pattern.rkt")
(require "../../share/version-information.rkt")
(require "../../share/dark-module.rkt")
(require "../../share/fill-data.rkt")
(require "../../share/data-encoding.rkt")
(require "../../share/mask-data.rkt")
(require "../../share/func.rkt")
(require "../../share/data-group.rkt")

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

;; return pair
;; car is squashed list
;; cdr is correpondent orignal place index
(define (squash-points points width)
  (let ([min_width (ceiling (* width 0.5))])
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
                       [original_str 
                        (foldr (lambda (a b) (string-append a b)) "" (map (lambda (b) (number->string b)) points_row))]
                       [squashed_str 
                        (foldr (lambda (a b) (string-append a b)) "" (map (lambda (b) (number->string b)) squashed_line))])

                  (if (regexp-match #rx"010111010" squashed_str)
                      (begin
                        (appTrace *TRACE_DEBUG* (lambda () (printf "as guess width:~a original line:~a\nsquashed line:~a\n" 
                                                                   guess_module_width original_str squashed_str)))
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
                (appTrace *TRACE_DEBUG* (lambda () (printf "row:~a guess_result:~a\n" row_index guess_result))))

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

(define (guess_result->pixel_map guess_result)
  (let ([pixel_map (make-hash)]
        [width (* (car guess_result) 7)])
    (let loop ([loop_list (cdr guess_result)])
      (when (not (null? loop_list))
            (let* ([finded_pattern (flatten (car loop_list))]
                   [row (first finded_pattern)])
              (let inner-loop ([col_loop_list (cdr finded_pattern)])
                (when (not (null? col_loop_list))
                      (hash-set! pixel_map (cons row (car col_loop_list)) '(24 255 0 255))
                      (let col-loop ([count 1])
                        (when (< count width)
                              (hash-set! pixel_map (cons row (+ (car col_loop_list) count)) '(0 0 255 255))
                              (col-loop (add1 count))))
                      (inner-loop (cdr col_loop_list))))

              (loop (cdr loop_list)))))
    pixel_map))

(define (align-matrix matrix fill)
  (let ([max_length 
         (apply max (map (lambda (row) (length row)) matrix))])
    (map
     (lambda (row)
       (if (< (length row) max_length)
           (append row (build-list (- max_length (length row)) (lambda (x) fill)))
           row))
     matrix)))

(define (trim-blank-lines matrix)
  (let loop ([loop_list matrix]
             [start #t]
             [result_list '()])
    (if (not (null? loop_list))
        (if start
            (if (andmap
                 (lambda (item)
                   (= item 0))
                 (car loop_list))
                (loop (cdr loop_list) #t result_list)
                (loop (cdr loop_list) #f (cons (car loop_list) result_list)))
            (loop (cdr loop_list) #f (cons (car loop_list) result_list)))
        (reverse result_list))))

(define (trim-matrix matrix)
  ;; trim four direction
  (matrix-row->col
   (trim-blank-lines
    (matrix-row->col
     (trim-blank-lines
      (matrix-row->col
       (trim-blank-lines
        (matrix-row->col 
         (trim-blank-lines 
          matrix)))))))))

(define (trim-tail matrix)
  (let loop ([loop_list matrix]
             [result_list '()]
             [finder_list '()]
             [pattern_count 0])
    (if (not (null? loop_list))
        (if (= pattern_count 2)
            (reverse (cdr result_list))
            (if (equal? finder_list 
                        '(
                          (1 1 1 1 1 1 1)
                          (1 0 0 0 0 0 1)
                          (1 0 1 1 1 0 1)
                          (1 0 1 1 1 0 1)
                          (1 0 1 1 1 0 1)
                          (1 0 0 0 0 0 1)
                          (1 1 1 1 1 1 1)
                          ))
                (loop (cdr loop_list) (cons (car loop_list) result_list) '() (add1 pattern_count))
                (if (= (length finder_list) 7)
                    (loop 
                     (cdr loop_list) 
                     (cons (car loop_list) result_list)
                     (cons (take (car loop_list) 7) (take finder_list 6))
                     pattern_count)
                    (loop 
                     (cdr loop_list)
                     (cons (car loop_list) result_list)
                     (cons (take (car loop_list) 7) finder_list)
                     pattern_count
                     ))))
        (reverse result_list))))

(define (squash-matrix matrix module_width)
  (let ([squash_matrix_x
         (map
          (lambda (row)
            (squash-points row module_width))
          matrix)])
    
    (let ([rotate_matrix (matrix-col->row (align-matrix squash_matrix_x 0))]
          [squash_matrix_y #f])

      (set! squash_matrix_y
            (map
             (lambda (row)
               (squash-points row module_width))
             rotate_matrix))

      (matrix-row->col (align-matrix squash_matrix_y 0)))))

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
  (inexact->exact
   (floor
    (sqrt (+ 
           (expt (- (car point_x) (car point_y)) 2)
           (expt (- (cdr point_x) (cdr point_y)) 2))))))

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

    (appTrace *TRACE_DEBUG* (lambda () 
                              (printf "step4 generate ref center points scan ref image\n")
                              (points->pic matrix "step4_center_points_scan.png" (guess_result->pixel_map guess_results))))
    
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

          (appTrace *TRACE_DEBUG* (lambda () (printf "step4 guess_results:~a\n" guess_results)))
          (appTrace *TRACE_DEBUG* (lambda () (printf "step4 group_map:~a\n" group_map)))
          (appTrace *TRACE_DEBUG* (lambda () (printf "step4 group_map first 3 group:~a\n" group_list)))
          (appTrace *TRACE_DEBUG* (lambda () (printf "step4 all_center_points:~a\n" all_center_points)))
    
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

          (appTrace *TRACE_DEBUG* (lambda () (printf "step4 points_distance_map:~a\n" points_distance_map)))

          (if (check-center-points-valid points_distance_map)
              (cons module_width (get-center-points points_distance_map))
              #f)))))

(define (calculate-rotate-ratio point_a point_b radius)
  (let ([point_a_x (car point_a)]
        [point_a_y (cdr point_a)]
        [point_b_x (car point_b)]
        [point_b_y (cdr point_b)]
        [matrix_count (* radius 2 4)]
        [move_count #f])
    
    (appTrace *TRACE_DEBUG* (lambda () (printf "step51:calculate rotate ratio:~a,~a,~a\n" point_a point_b radius)))
    
    (cond
     [(and
       (= point_b_x point_a_x)
       (> point_b_y point_a_y))
      (set! move_count 0)]
     [(and
       (> point_b_x point_a_x)
       (>= point_b_y (+ point_a_y radius)))
      (set! move_count (- point_b_x point_a_x))]
     [(and
       (>= point_b_x (+ point_a_x radius))
       (>= point_b_y point_a_y))
      (set! move_count (+ radius (- radius (- point_b_y point_a_y))))]
     [(and
       (>= point_b_x (+ point_a_x radius))
       (< point_b_y point_a_y))
      (set! move_count (+ (- point_a_y point_b_y) (* radius 2)))]
     [(and
       (<= point_b_y (- point_a_y radius))
       (>= point_b_x point_a_x))
      (set! move_count (+ (- radius (- point_b_x point_a_x)) (* radius 3)))]
     [(and
       (<= point_b_y (- point_a_y radius))
       (< point_b_x point_a_x))
      (set! move_count (* -1 (+ (- radius (- point_a_x point_b_x)) (* radius 3))))]
     [(and
       (<= point_b_x (- point_a_x radius))
       (<= point_b_y point_a_y))
      (set! move_count (* -1 (+ (- point_a_y point_b_y) (* radius 2))))]
     [(and
       (<= point_b_x (- point_a_x radius))
       (> point_b_y point_a_y))
      (set! move_count (* -1 (+ (- radius (- point_b_y point_a_y)) radius)))]
     [(and
       (>= point_b_y (+ point_a_y radius))
       (< point_b_x point_a_x))
      (set! move_count (* -1 (- point_a_x point_b_x)))]
     )
    
    (/ move_count matrix_count)))

(define (rotate-and-cut-bmp source_points ratio point_a distance_ab module_width)
  (let* ([source_pixels #f]
         [source_width (length (car source_points))]
         [source_height (length source_points)]
         [origin_bmp #f]
         [dest_width (+ distance_ab (* 12 module_width))]
         [dest_bmp (make-object bitmap% dest_width dest_width)]
         [dc (send dest_bmp make-dc)])
    (set! source_pixels (points->pixels source_points (make-hash)))
    (set! origin_bmp (make-object bitmap% source_width source_height))
    (send origin_bmp set-argb-pixels 0 0 source_width source_height source_pixels)
    (send dc translate 0 0)
    (send dc rotate (* (* pi 2) ratio))
    (send dc draw-bitmap origin_bmp (+ (* -1 (cdr point_a)) (* 6 module_width)) (+ (* -1 (car point_a)) (* 6 module_width)))
    (points->bw (bitmap->points dest_bmp) 127)))

(define (transform-points-list points_list start_point_pair)
  (map
   (lambda (point)
     (cons (+ (car start_point_pair) (sub1 (car point))) (+ (cdr start_point_pair) (sub1 (cdr point)))))
   points_list))

(define (exclude-finder-pattern width exclude_points_map)
  (for-each
   (lambda (start_point)
     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (transform-points-list (first (get-finder-pattern)) start_point))

     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (transform-points-list (second (get-finder-pattern)) start_point))

     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (transform-points-list (third (get-finder-pattern)) start_point)))
   (list
    (cons 0 0)
    (cons 0 (- width 7))
    (cons (- width 7) 0))))

(define (exclude-separator width exclude_points_map)
  (for-each
   (lambda (start_point)
     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
     (transform-points-list (first (get-separator)) '(0 . 0)))

     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (transform-points-list (second (get-separator)) (cons (- width 8) 0)))

     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (transform-points-list (third (get-separator)) (cons 0 (- width 8)))))
   (list
    (cons 0 0)
    (cons 0 (- width 7))
    (cons (- width 7) 0))))

(define (exclude-timing-pattern width exclude_points_map timing_points_map)
  (for-each
   (lambda (timing_points)
     (for-each
      (lambda (point_pair)
        (hash-set! timing_points_map (cons (sub1 (car point_pair)) (sub1 (cdr point_pair))) "timing")
        (hash-set! exclude_points_map (cons (sub1 (car point_pair)) (sub1 (cdr point_pair))) '(0 0 255 255)))
      timing_points))
   (get-timing-pattern-points width)))

(define (exclude-format-information width exclude_points_map)
  (for-each
   (lambda (point_pair)
     (hash-set! exclude_points_map point_pair '(0 0 255 255)))
   (transform-points-list (first (get-format-information)) '(0 . 0)))

  (for-each
   (lambda (point_pair)
     (hash-set! exclude_points_map point_pair '(0 0 255 255)))
   (transform-points-list (second (get-format-information)) (cons 0 (- width 8))))

  (for-each
   (lambda (point_pair)
     (hash-set! exclude_points_map point_pair '(0 0 255 255)))
   (transform-points-list (third (get-format-information)) (cons (- width 8) 0))))

(define (exclude-alignment-pattern version exclude_points_map timing_points_map)
  (for-each
   (lambda (center_point)
     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (foldr (lambda (a b) (quasiquote ((unquote-splicing a) (unquote-splicing b)))) '() 
             (fill-alignment-pattern-points (cons (sub1 (car center_point)) (sub1 (cdr center_point)))))))
   (get-alignment-pattern-center-points version exclude_points_map timing_points_map)))

(define (exclude-version version width exclude_points_map)
  (when (>= version 7)
        (for-each
         (lambda (point_pair)
           (hash-set! exclude_points_map point_pair '(0 0 255 255)))
         (transform-points-list (first (get-version-points)) (cons 0 (- width 11))))

          (for-each
           (lambda (point_pair)
             (hash-set! exclude_points_map point_pair '(0 0 255 255)))
           (transform-points-list (second (get-version-points)) (cons (- width 11) 0)))))

(define (exclude-dark-module version exclude_points_map)
  (let ([dark_point (get-dark-point version)])
    (hash-set! exclude_points_map (cons (sub1 (car dark_point)) (sub1 (cdr dark_point))) '(0 0 255 255))))

(define (get-data-head bits)
  (let ([mode #f]
        [mode1 #f]
        [mode2 #f]
        [remain_bits bits]
        [char_count #f]
        )

    (set! mode1 (get-indicator-mode (substring bits 0 4)))
    (set! remain_bits (substring remain_bits 4))
    
    (if (string=? mode1 "E")
        (let ([mode2 (get-indicator-mode (substring remain_bits 0 4))])
          (when mode2
                (set! mode (string-append mode1 mode2))
                (set! remain_bits (substring remain_bits 4))))
        (set! mode mode1))

    (if mode
        (list mode (string->number (substring remain_bits 0 8) 2) (substring remain_bits 8))
        #f)))

(define (qr-read pic_path)
  (let* ([step1_points_list #f]
         [original_height #f]
         [original_width #f]
         [step2_threshold #f]
         [step3_bw_points #f]
         [step4_pattern_center_points #f]
         [step5_rotate_ratio #f]
         [step6_rotated_points #f]
         [step7_trimed_points #f]
         [step8_squashed_points #f]
         [step9_end_points #f]
         [data_str ""]
         )

    (set! step1_points_list (pic->points pic_path))
    (set! original_width (length step1_points_list))
    (set! original_height (length (car step1_points_list)))
    (appTrace *TRACE_INFO* (lambda () (printf "step1:convert pic file to pixel points[~aX~a]\n" original_width original_height)))
    
    (set! step2_threshold (find-threshold step1_points_list))
    (appTrace *TRACE_INFO* (lambda () (printf "step2:find threshold is ~a\n" step2_threshold)))

    (set! step3_bw_points (points->bw step1_points_list step2_threshold))
    (appTrace *TRACE_INFO* (lambda () (printf "step3:use threshold convert pixel to points 0 or 1\n")))
    (appTrace *TRACE_DEBUG* (lambda () (points->pic step3_bw_points "step3_bw.png" (make-hash))))
    
    (set! step4_pattern_center_points (find-pattern-center-points step3_bw_points))
    (appTrace *TRACE_INFO* (lambda () (printf "step4 pattern center points:~a\n" step4_pattern_center_points)))

    (when step4_pattern_center_points
          (let ([pixel_map (make-hash)]
                [module_width (car step4_pattern_center_points)]
                [center_points (cdr step4_pattern_center_points)])
            (hash-set! pixel_map (first center_points) '(0 0 255 255))
            (hash-set! pixel_map (second center_points) '(0 255 0 255))
            (hash-set! pixel_map (third center_points) '(255 0 0 255))

            (appTrace *TRACE_DEBUG* (lambda () (points->pic step3_bw_points "step4_pattern_center.png" pixel_map)))
            
            (set! step5_rotate_ratio (calculate-rotate-ratio 
                                      (first center_points) 
                                      (second center_points) 
                                      (point-distance (first center_points) (second center_points))))
            (appTrace *TRACE_INFO* (lambda () (printf "step5 rotate ratio:~a\n" step5_rotate_ratio)))
            
            (if (= step5_rotate_ratio 0)
                (set! step6_rotated_points step3_bw_points)
                (set! step6_rotated_points
                      (rotate-and-cut-bmp
                       step3_bw_points
                       step5_rotate_ratio 
                       (first center_points) 
                       (point-distance (first center_points) (second center_points))
                       module_width)))

            (appTrace *TRACE_INFO* (lambda () (printf "step6 rotate and cut complete.\n")))
            
            (set! step7_trimed_points (trim-matrix step6_rotated_points))
            (appTrace *TRACE_DEBUG* (lambda () (points->pic step7_trimed_points "step7_trimed.png" (make-hash))))
            
            (set! step8_squashed_points (squash-matrix step7_trimed_points module_width))
            (appTrace *TRACE_DEBUG* 
                   (lambda ()
                     (points->pic step8_squashed_points "step8_squashed.png" (make-hash))
                     (printf "step8 points:\n")
                     (print-matrix step8_squashed_points)))

            (set! step9_end_points (trim-matrix (trim-tail step8_squashed_points)))
            (appTrace *TRACE_DEBUG*
                   (lambda ()
                     (points->pic step9_end_points "step9_end.png" (make-hash))
                     (printf "step9 points:\n")
                     (print-matrix step9_end_points)))
            
            (let* ([init_matrix step9_end_points]
                   [width (length (car init_matrix))]
                   [version #f]
                   [format_code_error_hash (get-code-error-hash)]
                   [format_information #f]
                   [error_level #f]
                   [mask_pattern #f]
                   [mask-proc #f]
                   [exclude_points_map (make-hash)]
                   [timing_points_map (make-hash)]
                   [new_exclude_points_map (make-hash)])

              (set! version (add1 (/ (- (length (car init_matrix)) 21) 4)))


              (set! format_information (hash-ref format_code_error_hash 
                                                 (foldr (lambda (a b) 
                                                          (string-append a b)) "" 
                                                          (map (lambda (item) (number->string item))
                                                               (reverse
                                                                (get-points init_matrix 
                                                                            (transform-points-list (first (get-format-information)) '(1 . 1))))))))
              (set! error_level (substring format_information 0 1))
              (set! mask_pattern (substring format_information 2 3))
              (appTrace *TRACE_INFO* 
                     (lambda () 
                       (printf "step9:width:~a, version:~a, format_information;~a, error_level:~a, mask_pattern:~a\n" 
                               width version format_information error_level mask_pattern)))
              (set! mask-proc (get-mask-proc (string->number mask_pattern)))

              (if (or (not (exact-nonnegative-integer? version)) (> version 40))
                  ""
                  (begin
                    (exclude-finder-pattern width exclude_points_map)
                    (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step91_exclude_finder_pattern.png" exclude_points_map)))
                    (exclude-separator width exclude_points_map)
                    (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step92_exclude_separator.png" exclude_points_map)))
                    (exclude-timing-pattern width exclude_points_map timing_points_map)
                    (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step93_exclude_timing_pattern.png" exclude_points_map)))
                    (exclude-alignment-pattern version exclude_points_map timing_points_map)
                    (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step94_exclude_alignment_pattern.png" exclude_points_map)))
                    (exclude-format-information width exclude_points_map)
                    (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step95_exclude_format_information.png" exclude_points_map)))
                    (exclude-version version width exclude_points_map)
                    (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step96_exclude_version.png" exclude_points_map)))
                    (exclude-dark-module version exclude_points_map)
                    (appTrace *TRACE_DEBUG* (lambda () (points->pic init_matrix "step97_exclude_dark_module.png" exclude_points_map)))
                    
                    (hash-for-each
                     exclude_points_map
                     (lambda (point val)
                       (hash-set! new_exclude_points_map (cons (add1 (car point)) (add1 (cdr point))) '(0 0 255 255))))

                    (let* ([trace_list (get-data-socket-list width #:skip_points_hash new_exclude_points_map)]
                           [data_list (get-points init_matrix trace_list)]
                           [unmask_data_bits #f]
                           [data_bits #f]
                           [mode #f])

                      (appTrace *TRACE_DEBUG* 
                             (lambda () 
                               (printf "mask data:~a\n" 
                                       (foldr (lambda (a b) (string-append a b)) "" 
                                              (map (lambda (item) (number->string item)) data_list)))))
                      (let* ([unmask_data (get-unmask-points init_matrix trace_list mask-proc)]
                             [data (car unmask_data)]
                             [mask_list (cdr unmask_data)])
                        (appTrace *TRACE_DEBUG* 
                               (lambda () (printf "mask list:~a\n" 
                                                  (foldr (lambda (a b) (string-append a b)) "" 
                                                         (map (lambda (item) (number->string item)) mask_list)))))
                        (set! unmask_data_bits (foldr (lambda (a b) (string-append a b)) "" 
                                                      (map (lambda (item) (number->string item)) data)))
                        (appTrace *TRACE_DEBUG* (lambda () (printf "unmask data:~a, [~a]\n" unmask_data_bits (string-length unmask_data_bits))))
                        (appTrace *TRACE_DEBUG* (lambda () (printf "group width:~a\n" (get-group-width version error_level))))
                        (set! data_bits (rearrange-data unmask_data_bits (defines->count-list (get-group-width version error_level))))

                        (appTrace *TRACE_DEBUG* (lambda () (printf "data:~a\n" data_bits)))
                        
                        (let ([mode (get-indicator-mode (substring data_bits 0 4))]
                              [data_count (string->number (substring data_bits 4 12) 2)])
                          (appTrace *TRACE_INFO* (lambda () (printf "head :mode:~a, data_count:~a\n" mode data_count)))
                          
                          (set! data_str
                                (bytes->string/utf-8 
                                 (list->bytes
                                  (map
                                   (lambda (rec)
                                     (string->number rec 2))
                                   (let loop ([loop_count data_count]
                                              [loop_str (substring data_bits 12)]
                                              [result_list '()])
                                     (if (> loop_count 0)
                                         (loop
                                          (sub1 loop_count)
                                          (substring loop_str 8)
                                          (cons (substring loop_str 0 8) result_list))
                                         (reverse result_list)))))))
                          (appTrace *TRACE_INFO* (lambda () (printf "data:~a\n" data_str)))))
                      ))))))
    data_str))
