#lang racket

(require racket/draw)

(provide (contract-out
          [version->modules (-> exact-nonnegative-integer? exact-nonnegative-integer?)]
          [locate-brick (-> exact-nonnegative-integer? pair? pair?)]
          [locate-finder-pattern (-> exact-nonnegative-integer? list?)]
          [draw-module (-> (is-a?/c bitmap-dc%) (or/c (is-a?/c color%) string?) pair? exact-nonnegative-integer? void?)]
          [draw-background (-> (is-a?/c bitmap-dc%) exact-nonnegative-integer? exact-nonnegative-integer? void?)]
          [draw-points (-> (is-a?/c bitmap-dc%) exact-nonnegative-integer? hash? void?)]
          [draw-debug-points (-> (is-a?/c bitmap-dc%) exact-nonnegative-integer? hash? void?)]
          [draw-block-points (-> (is-a?/c bitmap-dc%) exact-nonnegative-integer? hash? void?)]
          [transform-points-list (-> list? pair? list?)]
          [get-points-between (-> pair? pair? #:direction (or/c 'horizontal 'vertical) list?)]
          [add-terminator (-> string? exact-nonnegative-integer? string?)]
          [add-multi-eight (-> string? string?)]
          [repeat-right-pad-string (-> string? exact-nonnegative-integer? string? string?)]
          [split-bit-string-to-decimal (-> string? list?)]
          [split-decimal-list-on-contract (-> list? vector? list?)]
          [interleave-list (-> list? list?)]
          [decimal-list-to-string (-> list? string?)]
          [cut-string (-> string? list)]
          [*trace_level* parameter?]
          [trace (-> string? exact-nonnegative-integer? void?)]
          [add-point (-> pair? string? hash? hash? void?)]
          ))

(define *trace_level* (make-parameter 0))

(define (trace data trace_level)
  (when (>= (*trace_level*) trace_level)
        (printf "t[~a]=~a\n" trace_level data)))

(define (version->modules version)
  (if (and (>= version 1) (<= version 40))
      (+ 21 (* 4 (sub1 version)))
      (error "invalid version!")))

(define (draw-module dc color place_pair module_width)
  (when (not (string=? color "transparent"))
        (send dc set-pen color 1 'solid)
        (send dc set-brush color 'solid)

        (send dc draw-rectangle (car place_pair) (cdr place_pair) module_width module_width)))

(define (draw-points dc module_width points_map)
  (hash-for-each
   points_map
   (lambda (point_pair val)
     (let ([new_point_pair (cons (+ (car point_pair) 4) (+ (cdr point_pair) 4))])
       (if (string=? (car val) "1")
           (draw-module dc "black" (locate-brick module_width new_point_pair) module_width)
           (draw-module dc "white" (locate-brick module_width new_point_pair) module_width))))))

(define (draw-debug-points dc module_width points_map)
  (hash-for-each
   points_map
   (lambda (point_pair val)
     (let ([new_point_pair (cons (+ (car point_pair) 4) (+ (cdr point_pair) 4))])
       (if (string=? (car val) "1")
           (draw-debug-module dc "black" (locate-brick module_width new_point_pair) module_width)
           (draw-debug-module dc "white" (locate-brick module_width new_point_pair) module_width))))))

(define (draw-block-points dc module_width points_map)
  (hash-for-each
   points_map
   (lambda (point_pair val)
     (let ([new_point_pair (cons (+ (car point_pair) 4) (+ (cdr point_pair) 4))])
       (cond
        [(string=? (cdr val) "finder")
         (draw-module dc "red" (locate-brick module_width new_point_pair) module_width)]
        [(string=? (cdr val) "separator")
         (draw-module dc "green" (locate-brick module_width new_point_pair) module_width)]
        [(string=? (cdr val) "timing")
         (draw-module dc "brown" (locate-brick module_width new_point_pair) module_width)]
        [(string=? (cdr val) "alignment")
         (draw-module dc "blue" (locate-brick module_width new_point_pair) module_width)]
        [(string=? (cdr val) "format")
         (draw-module dc "teal" (locate-brick module_width new_point_pair) module_width)]
        [(string=? (cdr val) "version")
         (draw-module dc "magenta" (locate-brick module_width new_point_pair) module_width)]
        [(string=? (cdr val) "dark")
         (draw-module dc "pink" (locate-brick module_width new_point_pair) module_width)]
        [(string=? (cdr val) "data")
         (draw-module dc "black" (locate-brick module_width new_point_pair) module_width)])))))

;; draw the background, help to count module
(define (draw-background dc modules module_width)
  (let loop-row ([row 1])
    (when (<= row modules)
          (let loop-col ([col 1])
            (when (<= col modules)
                  (draw-module dc "white" (locate-brick module_width (cons row col)) module_width)
                  (loop-col (add1 col))))
          (loop-row (add1 row)))))

(define (draw-debug-module dc color place_pair module_width)
  (if (string=? color "black")
      (send dc set-pen "white" 1 'dot)
      (send dc set-pen "black" 1 'dot))
      
  (send dc set-brush color 'solid)

  (send dc draw-rectangle (car place_pair) (cdr place_pair) module_width module_width))

(define (locate-brick module_width place_pair)
  (cons (* (sub1 (cdr place_pair)) module_width)
        (* (sub1 (car place_pair)) module_width)))

(define (locate-finder-pattern modules)
  (list
   '(1 . 1)
   (cons (add1 (- modules 7)) 1)
   (cons 1 (add1 (- modules 7)))))

(define (transform-points-list points_list start_point_pair)
  (map
   (lambda (point)
     (cons (+ (car start_point_pair) (sub1 (car point))) (+ (cdr start_point_pair) (sub1 (cdr point)))))
   points_list))

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

(define (add-terminator content limit_length)
  (let* ([content_length (string-length content)]
         [gap (- limit_length content_length)])
    (if (<= gap 0)
        content
        (if (<= gap 4)
            (~a content #:min-width (+ content_length gap) #:right-pad-string "0")
            (~a content #:min-width (+ content_length 4) #:right-pad-string "0")))))

(define (add-multi-eight content)
  (let* ([content_length (string-length content)]
         [eight_length (* 8 (ceiling (/ content_length 8)))])
    (~a content #:min-width eight_length #:right-pad-string "0")))

(define (repeat-right-pad-string content limit_length pad_str)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_content content])
        (if (>= (string-length loop_content) limit_length)
            (printf "~a" loop_content)
            (let loop_inner ([inner_loop_content loop_content]
                             [pad_list (string->list pad_str)])
              (if (not (null? pad_list))
                  (if (>= (string-length inner_loop_content) limit_length)
                      (printf "~a" inner_loop_content)
                      (loop_inner (format "~a~a" inner_loop_content (car pad_list)) (cdr pad_list)))
                  (loop inner_loop_content))))))))

(define (split-bit-string-to-decimal bit_str)
  (reverse
   (let loop ([loop_str bit_str]
              [result_list '()])
     (if (not (string=? loop_str ""))
         (loop (substring loop_str 8) (cons (string->number (string-append "#b" (substring loop_str 0 8))) result_list))
         result_list))))

(define (split-decimal-list-on-contract num_list contract_vec)
  (let ([group1_block_count (car (vector-ref contract_vec 0))]
        [group1_count_per_block (cdr (vector-ref contract_vec 0))]
        [group2_block_count (car (vector-ref contract_vec 1))]
        [group2_count_per_block (cdr (vector-ref contract_vec 1))]
        [remain_list #f])

    (list
     (let loop ([loop_list num_list]
                [loop_block_count group1_block_count]
                [loop_count group1_count_per_block]
                [temp_result_list '()]
                [result_list '()])
       (if (= loop_block_count 0)
           (begin
             (set! remain_list loop_list)
             (reverse result_list))
           (if (= loop_count 1)
               (loop (cdr loop_list) (sub1 loop_block_count) group1_count_per_block '() (cons (reverse (cons (car loop_list) temp_result_list)) result_list))
               (loop (cdr loop_list) loop_block_count (sub1 loop_count) (cons (car loop_list) temp_result_list) result_list))))

     (let loop ([loop_list remain_list]
                [loop_block_count group2_block_count]
                [loop_count group2_count_per_block]
                [temp_result_list '()]
                [result_list '()])
       (if (= loop_block_count 0)
           (reverse result_list)
           (if (= loop_count 1)
               (loop (cdr loop_list) (sub1 loop_block_count) group2_count_per_block '() (cons (reverse (cons (car loop_list) temp_result_list)) result_list))
               (loop (cdr loop_list) loop_block_count (sub1 loop_count) (cons (car loop_list) temp_result_list) result_list)))))))

(define (interleave-list data_list)
  (let loop ([count 0]
             [result_list '()])
    (let ([temp_list '()])
      (for-each
       (lambda (item_list)
         (when (<= count (sub1 (length item_list)))
               (set! temp_list `(,@temp_list ,(list-ref item_list count)))))
       data_list)
      
      (if (null? temp_list)
          result_list
          (loop (add1 count) `(,@result_list ,@temp_list))))))

(define (decimal-list-to-string decimal_list)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (num)
         (printf "~a" (~r num #:base 2 #:min-width 8 #:pad-string "0")))
       decimal_list))))

(define (cut-string str)
  (reverse
   (let loop ([loop_str str]
              [result_list '()])
     (cond 
      [(> (string-length loop_str) 8)
       (loop (substring loop_str 8) (cons (substring loop_str 0 8) result_list))]
      [(> (string-length loop_str) 0)
       (cons loop_str result_list)]
      [else
       result_list]))))

(define (add-point point type points_map type_map)
  (hash-set! points_map point 
