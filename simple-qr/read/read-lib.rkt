#lang racket

(require "../../share/func.rkt")

(provide (contract-out
          [find-threshold (-> list? exact-nonnegative-integer?)]
          [points->bw (-> list? exact-nonnegative-integer? list?)]
          [guess-first-dark-width (-> list? exact-nonnegative-integer?)]
          [guess-module-width (-> (or/c #f exact-nonnegative-integer?) list? (or/c boolean? list?))]
          [squash-points (-> list? exact-nonnegative-integer? pair?)]
          [guess_result->pixel_map (-> list? hash?)]
          [squash-matrix (-> (listof list?) exact-nonnegative-integer? (listof list?))]
          [point-distance (-> pair? pair? number?)]
          [find-pattern-center-points (-> (listof list?) (or/c boolean? pair?))]
          [check-center-points-valid (-> hash? boolean?)]
          [get-center-points (-> hash? list?)]
          [calculate-rotate-ratio (-> pair? pair? exact-nonnegative-integer? number?)]
          [align-matrix (-> (listof list?) any/c (listof list?))]
          [trim-matrix (-> (listof list?) (listof list?))]
          [trim-tail (-> (listof list?) (listof list?))]
          [rotate-and-cut-bmp (-> (listof list?) number? pair? number? number? list?)]
          [exclude-finder-pattern (-> exact-nonnegative-integer? hash? void?)]
          [exclude-separator (-> exact-nonnegative-integer? hash? void?)]
          [exclude-timing-pattern (-> exact-nonnegative-integer? hash? hash? void?)]
          [exclude-alignment-pattern (-> exact-nonnegative-integer? hash? hash? void?)]
          [exclude-format-information (-> exact-nonnegative-integer? hash? void?)]
          [exclude-version (-> exact-nonnegative-integer? exact-nonnegative-integer? hash? void?)]
          [exclude-dark-module (-> exact-nonnegative-integer? hash? void?)]
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
          (floor (/ (+ max_value min_value) 2))))))

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
               [last_index 0]
               [last_value -1]
               [same_count 0]
               [result_list '()]
               [index_list '()])

      (if (not (null? loop_points))
          (if (= same_count width)
              (loop (cdr loop_points) (+ width last_index) (car loop_points) 1 (cons last_value result_list) (cons last_index index_list))
              (if (= (car loop_points) last_value)
                  (loop (cdr loop_points) last_index last_value (add1 same_count) result_list index_list)
                  (if (= last_value -1)
                      (loop (cdr loop_points) last_index (car loop_points) (add1 same_count) result_list index_list)
                      (if (>= same_count min_width)
                          (loop (cdr loop_points) (+ last_index same_count) (car loop_points) 1 (cons last_value result_list) (cons last_index index_list))
                          (loop (cdr loop_points) (+ last_index same_count) (car loop_points) 1 result_list index_list)))))
          (if (and (> same_count 0) (>= same_count min_width))
              (cons (reverse (cons last_value result_list)) (reverse (cons last_index index_list)))
              (cons (reverse result_list) (reverse index_list)))))))

(define (guess-module-width guess_module_width points_row)
  (let ([max_module_width (floor (/ (length points_row) 14))])
    (let loop ([points points_row])
      (if (not (null? points))
          (if (= (car points) 1)
              (begin
                (when (not guess_module_width)
                      (set! guess_module_width (guess-first-dark-width points)))

                (let* ([squashed_line (squash-points points_row guess_module_width)]
                       [squashed_cols (car squashed_line)]
                       [squashed_positions (cdr squashed_line)]
                       [original_str 
                        (foldr (lambda (a b) (string-append a b)) "" (map (lambda (b) (number->string b)) points_row))]
                       [squashed_str 
                        (foldr (lambda (a b) (string-append a b)) "" (map (lambda (b) (number->string b)) squashed_cols))])

                  (if (regexp-match #rx"010111010" squashed_str)
                      (cons
                       guess_module_width
                       (map
                        (lambda (item)
                          (list-ref squashed_positions (add1 (car item))))
                        (regexp-match-positions* #rx"010111010" squashed_str)))
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

(define (trim-noise-lines matrix)
  (let loop ([loop_list matrix]
             [start #t]
             [result_list '()])
    
    (if (not (null? loop_list))
        (if start
            (let* ([count_zero 
                    (let inner-loop ([loop_items (car loop_list)]
                                     [result 0])
                      (if (not (null? loop_items))
                          (if (= (car loop_items) 0)
                              (inner-loop (cdr loop_items) (add1 result))
                              (inner-loop (cdr loop_items) result))
                          result))]
                   [bili (/ count_zero (length (car loop_list)))])
              (cond
               [(> bili 0.9)
                (loop (cdr loop_list) #t result_list)]
               [else
                (loop (cdr loop_list) #f (cons (car loop_list) result_list))]))
            (loop (cdr loop_list) #f (cons (car loop_list) result_list)))
        (reverse result_list))))

(define (trim-matrix matrix)
  ;; trim four direction
  (matrix-row->col
   (trim-noise-lines
    (matrix-row->col
     (trim-noise-lines
      (matrix-row->col
       (trim-noise-lines
        (matrix-row->col 
         (trim-noise-lines matrix)))))))))

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
            (car (squash-points row module_width)))
          matrix)])
    
    (let ([rotate_matrix (matrix-col->row (align-matrix squash_matrix_x 0))]
          [squash_matrix_y #f])

      (set! squash_matrix_y
            (map
             (lambda (row)
               (car (squash-points row module_width)))
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

(define (exclude-finder-pattern modules exclude_points_map)
  (let ([finder_pattern (get-finder-pattern)])
    (for-each
     (lambda (start_point)
       (for-each
        (lambda (point_pair)
          (hash-set! exclude_points_map point_pair "Orchid"))
        (transform-points-list (first finder_pattern) start_point))

       (for-each
        (lambda (point_pair)
          (hash-set! exclude_points_map point_pair "Orchid"))
        (transform-points-list (second finder_pattern) start_point))

       (for-each
        (lambda (point_pair)
          (hash-set! exclude_points_map point_pair "Orchid"))
        (transform-points-list (third finder_pattern) start_point)))
     (locate-finder-pattern modules))))

(define (exclude-separator modules exclude_points_map)
  (let ([separator_list (get-separator)]
        [finder_pattern_points (locate-finder-pattern modules)])
    (for-each
     (lambda (point_pair)
       (hash-set! exclude_points_map point_pair "Olive"))
     (transform-points-list (first separator_list) (first finder_pattern_points)))

    (for-each
     (lambda (point_pair)
       (hash-set! exclude_points_map point_pair "Olive"))
     (transform-points-list (second separator_list) (move-point-col (second finder_pattern_points) -1)))

    (for-each
     (lambda (point_pair)
       (hash-set! exclude_points_map point_pair "Olive"))
     (transform-points-list (third separator_list) (move-point-row (third finder_pattern_points) -1)))))

(define (exclude-timing-pattern modules exclude_points_map timing_points_map)
  (for-each
   (lambda (timing_points)
     (for-each
      (lambda (point_pair)
        (hash-set! timing_points_map point_pair "timing")
        (hash-set! exclude_points_map point_pair "Lime"))
      timing_points))
   (get-timing-pattern-points modules)))

(define (exclude-alignment-pattern version exclude_points_map timing_points_map)
  (for-each
   (lambda (center_point)
     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair "RoyalBlue"))
      (foldr (lambda (a b) (quasiquote ((unquote-splicing a) (unquote-splicing b)))) '()
             (fill-alignment-pattern-points center_point))))
   (get-alignment-pattern-center-points version exclude_points_map timing_points_map)))

(define (exclude-format-information modules exclude_points_map)
  (let ([format_information_points (get-format-information)]
        [finder_pattern_points (locate-finder-pattern modules)])
    (for-each
     (lambda (point_pair)
       (hash-set! exclude_points_map point_pair "Purple"))
     (transform-points-list (first format_information_points) (first finder_pattern_points)))

    (for-each
     (lambda (point_pair)
       (hash-set! exclude_points_map point_pair "Purple"))
     (transform-points-list (second format_information_points) (move-point-col (second finder_pattern_points) -1)))

    (for-each
     (lambda (point_pair)
       (hash-set! exclude_points_map point_pair "Purple"))
     (transform-points-list (third format_information_points) (move-point-row (third finder_pattern_points) -1)))))

(define (exclude-version version modules exclude_points_map)
  (let ([version_points (get-version-points)])
    (when (>= version 7)
          (for-each
           (lambda (point_pair)
             (hash-set! exclude_points_map point_pair "Yellow"))
           (transform-points-list (first version_points) (cons 1 (- modules 10))))

          (for-each
           (lambda (point_pair)
             (hash-set! exclude_points_map point_pair "Yellow"))
           (transform-points-list (second version_points) (cons (- modules 10) 1))))))

(define (exclude-dark-module version exclude_points_map)
  (let ([dark_point (get-dark-point version)])
    (hash-set! exclude_points_map dark_point "Cyan")))

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

