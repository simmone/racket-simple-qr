#lang racket

(require "../func/func.rkt")
(require "../../../share/format-information.rkt")

(require racket/format)

(provide (contract-out
          [draw-reserved-format-information (-> exact-nonnegative-integer? hash? hash? void?)]
          [draw-format-information (-> string?
                                       exact-nonnegative-integer?
                                       exact-nonnegative-integer?
                                       hash?
                                       hash?
                                       void?)]
          ))

(define *information_points*
  '(
    (                                                                (1 . 9)
                                                                     (2 . 9)
                                                                     (3 . 9)
                                                                     (4 . 9)
                                                                     (5 . 9)
                                                                     (6 . 9)

                                                                     (8 . 9)
     (9 . 9) (9 . 8) (9 . 6) (9 . 5) (9 . 4) (9 . 3)         (9 . 2) (9 . 1))

    ((9 . 8) (9 . 7) (9 . 6) (9 . 5) (9 . 4) (9 . 3) (9 . 2) (9 . 1))

    (                                                                
                                                                     (2 . 9)
                                                                     (3 . 9)
                                                                     (4 . 9)
                                                                     (5 . 9)
                                                                     (6 . 9)
                                                                     (7 . 9)
                                                                     (8 . 9))
    ))

(define (draw-reserved-format-information modules points_map type_map)
  (let* ([finder_pattern_start_points (locate-finder-pattern modules)]
         [top_left_point (first finder_pattern_start_points)]
         [bottom_left_point (second finder_pattern_start_points)]
         [top_right_point (third finder_pattern_start_points)]
         [new_bottom_left_point (cons (sub1 (car bottom_left_point)) (cdr bottom_left_point))]
         [new_bottom_point (cons (car top_right_point) (sub1 (cdr top_right_point)))])
    (for-each
     (lambda (point_pair)
       (add-point point_pair "0" "format" points_map type_map))
     (transform-points-list (first *information_points*) top_left_point))

     (for-each
      (lambda (point_pair)
       (add-point point_pair "0" "format" points_map type_map))
      (transform-points-list (second *information_points*) new_bottom_point))

     (for-each
      (lambda (point_pair)
       (add-point point_pair "0" "format" points_map type_map))
      (transform-points-list (third *information_points*) new_bottom_left_point))))

(define (draw-format-information error_level mask_number modules points_map type_map)
  (let* ([finder_pattern_start_points (locate-finder-pattern modules)]
         [top_left_point (first finder_pattern_start_points)]
         [bottom_left_point (second finder_pattern_start_points)]
         [top_right_point (third finder_pattern_start_points)]
         [new_bottom_left_point (cons (sub1 (car bottom_left_point)) (cdr bottom_left_point))]
         [new_bottom_point (cons (car top_right_point) (sub1 (cdr top_right_point)))]
         [format_string (hash-ref (get-error-code-hash) (string-append error_level "-" (number->string mask_number)))])

    (let loop ([data_list (reverse (string->list format_string))]
               [trace_list (transform-points-list (first *information_points*) top_left_point)])
      (when (and (not (null? data_list)) (not (null? trace_list)))
            (if (char=? (car data_list) #\0)
                (add-point (car trace_list) "0" "format" points_map type_map)
                (add-point (car trace_list) "1" "format" points_map type_map))
            (loop (cdr data_list) (cdr trace_list))))

    (let loop ([data_list (reverse (string->list format_string))]
               [trace_list 
                `(,@(transform-points-list (second *information_points*) new_bottom_point)
                  ,@(transform-points-list (third *information_points*) new_bottom_left_point))])
      (when (and (not (null? data_list)) (not (null? trace_list)))
            (if (char=? (car data_list) #\0)
                (add-point (car trace_list) "0" "format" points_map type_map)
                (add-point (car trace_list) "1" "format" points_map type_map))
            (loop (cdr data_list) (cdr trace_list))))))
