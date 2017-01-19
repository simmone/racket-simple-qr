#lang racket

(provide (contract-out
          [matrix->square (->* ((listof list?)) (#:fill any/c) list?)]
          [matrix->circles (-> (listof list?) list?)]
          [circles->matrix (-> list? (listof list?))]
          [shift-list (-> list? exact-integer? list?)]
          [print-matrix (-> (listof list?) void?)]
          [matrix-rotate (->* ((listof list?) number?) (#:fill any/c) (listof list?))]
          [matrix-row->col (-> (listof list?) (listof list?))]
          [matrix-col->row (-> (listof list?) (listof list?))]
          ))

(define (matrix->square matrix #:fill [fill #f])
  (let* ([max_width (max (length matrix) (length (car matrix)))]
         [matrix_width (if (= (remainder max_width 2) 1) max_width (add1 max_width))]
         [col_pre_fill_count (floor (/ (- matrix_width (length (car matrix))) 2))]
         [col_suffix_fill_count (- matrix_width (length (car matrix)) col_pre_fill_count)]
         [row_pre_fill_count (floor (/ (- matrix_width (length matrix)) 2))]
         [row_suffix_fill_count (- matrix_width (length matrix) row_pre_fill_count)])
    
    (append
     (build-list row_pre_fill_count (lambda (x) (build-list matrix_width (lambda (x) fill))))

     (let row-loop ([loop_row_list matrix]
                    [row_result_list '()])
       (if (not (null? loop_row_list))
           (row-loop (cdr loop_row_list)
                     (cons
                      (append
                       (build-list col_pre_fill_count (lambda (x) fill))

                       (car loop_row_list)

                       (build-list col_suffix_fill_count (lambda (x) fill)))
                      row_result_list))
           (reverse row_result_list)))

     (build-list row_suffix_fill_count (lambda (x) (build-list matrix_width (lambda (x) fill)))))
    ))

;; matrix is a square

(define (matrix->circles matrix)
  (let ([radius (floor (/ (length matrix) 2))])
    (append
     (let loop ([loop_radius radius]
                [circles '()])
       (if (> loop_radius 0)
           (loop (sub1 loop_radius)
                 (cons
                  (append
                   (let up-loop ([loop_y (- radius loop_radius)]
                                 [up_list '()])
                     (if (< loop_y (+ radius loop_radius))
                         (up-loop
                          (add1 loop_y)
                          (cons (list-ref (list-ref matrix (- radius loop_radius)) loop_y) up_list))
                         (reverse up_list)))

                   (let right-loop ([loop_x (- radius loop_radius)]
                                    [right_list '()])
                     (if (< loop_x (+ radius loop_radius))
                         (right-loop
                          (add1 loop_x)
                          (cons (list-ref (list-ref matrix loop_x) (+ radius loop_radius)) right_list))
                         (reverse right_list)))

                   (let down-loop ([loop_y (+ radius loop_radius)]
                                   [down_list '()])
                     (if (> loop_y (- radius loop_radius))
                         (down-loop 
                          (sub1 loop_y)
                          (cons (list-ref (list-ref matrix (+ radius loop_radius)) loop_y) down_list))
                         (reverse down_list)))

                   (let left-loop ([loop_x (+ radius loop_radius)]
                                   [left_list '()])
                     (if (> loop_x (- radius loop_radius))
                         (left-loop
                          (sub1 loop_x)
                          (cons (list-ref (list-ref matrix loop_x) (- radius loop_radius)) left_list))
                         (reverse left_list)))
                   )
                  circles))
           (reverse circles)))
     (list (list (list-ref (list-ref matrix radius) radius))))))

(define (circles->matrix circles)
  (let* ([radius (sub1 (length circles))]
         [matrix_length (add1 (* radius 2))]
         [matrix (make-hash)])
    
    (let row-loop ([row_index 0])
      (when (< row_index matrix_length)
            (let ([col_map (make-hash)])
              (let col-loop ([col_index 0])
                (when (< col_index matrix_length)
                      (hash-set! col_map col_index #f)
                      (col-loop (add1 col_index))))
              (hash-set! matrix row_index col_map))
            (row-loop (add1 row_index))))
    
    ;; set center point
    (hash-set! (hash-ref matrix radius) radius (car (last circles)))

    (let loop ([loop_circles circles]
               [loop_radius radius])
      (when (not (null? loop_circles))
            (let ([circle (car loop_circles)])
              (let up-loop ([up_loop_items circle]
                            [loop_y (- radius loop_radius)])
                (if (< loop_y (+ radius loop_radius))
                    (begin
                      (hash-set! (hash-ref matrix (- radius loop_radius)) loop_y (car up_loop_items))
                      (up-loop (cdr up_loop_items) (add1 loop_y)))
                    (let right-loop ([right_loop_items up_loop_items]
                                     [loop_x (- radius loop_radius)])
                      (if (< loop_x (+ radius loop_radius))
                          (begin
                            (hash-set! (hash-ref matrix loop_x) (+ radius loop_radius) (car right_loop_items))
                            (right-loop (cdr right_loop_items) (add1 loop_x)))
                          (let down-loop ([down_loop_items right_loop_items]
                                          [loop_y (+ radius loop_radius)])
                            (if (> loop_y (- radius loop_radius))
                                (begin
                                  (hash-set! (hash-ref matrix (+ radius loop_radius)) loop_y (car down_loop_items))
                                  (down-loop (cdr down_loop_items) (sub1 loop_y)))
                                (let left-loop ([left_loop_items down_loop_items]
                                                [loop_x (+ radius loop_radius)])
                                  (when (> loop_x (- radius loop_radius))
                                        (hash-set! (hash-ref matrix loop_x) (- radius loop_radius) (car left_loop_items))
                                        (left-loop (cdr left_loop_items) (sub1 loop_x)))
                                  ))))))))
            (loop (cdr loop_circles) (sub1 loop_radius))))
    
    (map
     (lambda (row)
       (map
        (lambda (cols)
          (cdr cols))
        (sort (hash->list (cdr row)) (lambda (a b) (< (car a) (car b))))))
     (sort (hash->list matrix) (lambda (a b) (< (car a) (car b)))))))

(define (shift-list origin_list shift_width)
  (let ([list_length (length origin_list)]
        [abs_shift_width (abs shift_width)])
    (let loop ([loop_list (if (< shift_width 0) (reverse origin_list) origin_list)]
               [index 0]
               [result_list '()])
      (if (not (null? loop_list))
          (loop 
           (cdr loop_list)
           (add1 index)
           (cons
            (cons 
             (remainder (+ index abs_shift_width) list_length)
             (car loop_list))
            result_list))
          (let ([target_list
                 (map
                  (lambda (item)
                    (cdr item))
                  (sort
                   result_list
                   (lambda (a b)
                     (< (car a) (car b)))))])
            (if (< shift_width 0) (reverse target_list) target_list))))))

(define (print-matrix matrix)
  (for-each
   (lambda (row)
     (for-each
      (lambda (col)
        (printf "~a" (~a #:width 2 #:align 'right #:pad-string "0" col)))
      row)
     (printf "\n"))
   matrix))

(define (matrix-rotate matrix rotate_ratio #:fill [fill #f])
  (let* ([square_matrix (matrix->square matrix #:fill fill)]
         [circles (matrix->circles square_matrix)]
         [outtest_circle_length (length (car circles))]
         [rotate_number (inexact->exact (floor (* outtest_circle_length rotate_ratio)))])
    (circles->matrix
     (let loop ([loop_circles circles]
                [new_circles '()])
       (if (not (null? loop_circles))
           (loop
            (cdr loop_circles)
            (cons
             (shift-list (car loop_circles) (floor (* (/ (length (car loop_circles)) outtest_circle_length) rotate_number)))
             new_circles))
           (reverse new_circles))))))

(define (matrix-row->col matrix)
  (let ([matrix_width (length (car matrix))]
        [matrix_height (length matrix)])
    (let loop-row ([index 0]
                   [result_list '()])
      (if (< index matrix_width)
          (loop-row (add1 index)
                (cons
                 (let loop-col ([rows matrix]
                                [row '()])
                   (if (not (null? rows))
                       (loop-col
                        (cdr rows)
                        (cons
                         (list-ref (car rows) index)
                         row))
                       row))
                 result_list))
          (reverse result_list)))))

(define (matrix-col->row matrix)
  (let ([matrix_width (length (car matrix))]
        [matrix_height (length matrix)])
    (let loop-row ([index (sub1 matrix_width)]
                   [result_list '()])
      (if (>= index 0)
          (loop-row (sub1 index)
                (cons
                 (let loop-col ([rows matrix]
                                [row '()])
                   (if (not (null? rows))
                       (loop-col
                        (cdr rows)
                        (cons
                         (list-ref (car rows) index)
                         row))
                       (reverse row)))
                 result_list))
          (reverse result_list)))))

