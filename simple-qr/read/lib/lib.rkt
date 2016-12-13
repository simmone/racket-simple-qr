#lang racket

(provide (contract-out
          [pic->points (-> path-string? list?)]
          [find-threshold (-> list? exact-nonnegative-integer?)]
          [points->bw (-> list? exact-nonnegative-integer? list?)]
          [print-points (-> list? void?)]
          [matrix-supply (->* (list?) (#:fill any/c) list?)]
          ))

(require racket/draw)

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

(define (matrix-supply matrix #:fill [fill_thing #f])
  (let* ([matrix_width (length matrix)]
         [supplied_matrix_width (if (= (remainder matrix_width 2) 1) matrix_width (add1 matrix_width))]
         [matrix_height (length (car matrix))]
         [supplied_matrix_height (if (= (remainder matrix_height 2) 1) matrix_height (add1 matrix_height))])
    (let row-loop ([loop_row_list matrix]
                   [row_result_list '()])
      (if (not (null? loop_row_list))
          (row-loop (cdr loop_row_list)
                    (cons
                     (let col-loop ([loop_col_list (car loop_row_list)]
                                    [col_result_list '()])
                       (if (not (null? loop_col_list))
                           (col-loop (cdr loop_col_list) (cons (car loop_col_list) col_result_list))
                           (if (> supplied_matrix_width (length col_result_list))
                               (reverse (cons fill_thing col_result_list))
                               (reverse col_result_list))))
                     row_result_list))
          (if (> supplied_matrix_height (length row_result_list))
              (reverse (cons (build-list supplied_matrix_width (lambda (x) fill_thing)) row_result_list))
              (reverse row_result_list))))))
    
