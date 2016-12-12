#lang racket

(require "../func/func.rkt")

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


(define *error_level_code_hash*
  '#hash(
         ("L-0".  "111011111000100")
         ("L-1" . "111001011110011")
         ("L-2" . "111110110101010")
         ("L-3" . "111100010011101")
         ("L-4" . "110011000101111")
         ("L-5" . "110001100011000")
         ("L-6" . "110110001000001")
         ("L-7" . "110100101110110")
         ("M-0" . "101010000010010")
         ("M-1" . "101000100100101")
         ("M-2" . "101111001111100")
         ("M-3" . "101101101001011")
         ("M-4" . "100010111111001")
         ("M-5" . "100000011001110")
         ("M-6" . "100111110010111")
         ("M-7" . "100101010100000")
         ("Q-0" . "011010101011111")
         ("Q-1" . "011000001101000")
         ("Q-2" . "011111100110001")
         ("Q-3" . "011101000000110")
         ("Q-4" . "010010010110100")
         ("Q-5" . "010000110000011")
         ("Q-6" . "010111011011010")
         ("Q-7" . "010101111101101")
         ("H-0" . "001011010001001")
         ("H-1" . "001001110111110")
         ("H-2" . "001110011100111")
         ("H-3" . "001100111010000")
         ("H-4" . "000011101100010")
         ("H-5" . "000001001010101")
         ("H-6" . "000110100001100")
         ("H-7" . "000100000111011")))
         
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
         [format_string (hash-ref *error_level_code_hash* (string-append error_level "-" (number->string mask_number)))])

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
