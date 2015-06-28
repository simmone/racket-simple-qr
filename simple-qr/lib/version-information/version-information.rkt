#lang racket

(require "../func/func.rkt")

(provide (contract-out
          [draw-reserved-version-information (-> exact-nonnegative-integer? exact-nonnegative-integer? hash? void?)]
          [draw-version-information (-> exact-nonnegative-integer? exact-nonnegative-integer? hash? void?)]
          ))

(define *version_points*
  '(
    ((1 . 1) (1 . 2) (1 . 3)
     (2 . 1) (2 . 2) (2 . 3)
     (3 . 1) (3 . 2) (3 . 3)
     (4 . 1) (4 . 2) (4 . 3)
     (5 . 1) (5 . 2) (5 . 3)
     (6 . 1) (6 . 2) (6 . 3))
    ((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6)
     (2 . 1) (2 . 2) (2 . 3) (2 . 4) (2 . 5) (2 . 6)
     (3 . 1) (3 . 2) (3 . 3) (3 . 4) (3 . 5) (3 . 6))
    ))

(define (draw-reserved-version-information version modules points_map)
  (when (>= version 7)
        (let* ([finder_pattern_start_points (locate-finder-pattern modules)]
               [bottom_left_point (second finder_pattern_start_points)]
               [top_right_point (third finder_pattern_start_points)]
               [new_bottom_left_point (cons (- (car bottom_left_point) 4) (cdr bottom_left_point))]
               [new_top_right_point (cons (car top_right_point) (- (cdr top_right_point) 4))])
          (for-each
           (lambda (point_pair)
             (hash-set! points_map point_pair '("0" . "version")))
           (transform-points-list (first *version_points*) new_top_right_point))

          (for-each
           (lambda (point_pair)
             (hash-set! points_map point_pair '("0" . "version")))
           (transform-points-list (second *version_points*) new_bottom_left_point)))))

(define *trace_points*
  '(
    ((1 . 1) (2 . 1) (3 . 1) (1 . 2) (2 . 2) (2 . 3) (1 . 3) (2 . 3) (3 . 3) (1 . 4) (2 . 4) (3 . 4) (1 . 5) (2 . 5) (3 . 5) (1 . 6) (2 . 6) (3 . 6))
    ((1 . 1) (1 . 2) (1 . 3) (2 . 1) (2 . 2) (2 . 3) (3 . 1) (3 . 2) (3 . 3) (4 . 1) (4 . 2) (4 . 3) (5 . 1) (5 . 2) (5 . 3) (6 . 1) (6 . 2) (6 . 3))))

(define *version_code_hash*
  '#hash(
         (7 . "000111110010010100")
         (8 . "001000010110111100")
         (9 . "001001101010011001")
         (10 . "001010010011010011")
         (11 . "001011101111110110")
         (12 . "001100011101100010")
         (13 . "001101100001000111")
         (14 . "001110011000001101")
         (15 . "001111100100101000")
         (16 . "010000101101111000")
         (17 . "010001010001011101")
         (18 . "010010101000010111")
         (19 . "010011010100110010")
         (20 . "010100100110100110")
         (21 . "010101011010000011")
         (22 . "010110100011001001")
         (23 . "010111011111101100")
         (24 . "011000111011000100")
         (25 . "011001000111100001")
         (26 . "011010111110101011")
         (27 . "011011000010001110")
         (28 . "011100110000011010")
         (29 . "011101001100111111")
         (30 . "011110110101110101")
         (31 . "011111001001010000")
         (32 . "100000100111010101")
         (33 . "100001011011110000")
         (34 . "100010100010111010")
         (35 . "100011011110011111")
         (36 . "100100101100001011")
         (37 . "100101010000101110")
         (38 . "100110101001100100")
         (39 . "100111010101000001")
         (40 . "101000110001101001")))

(define (draw-version-information version modules points_map)
  (when (>= version 7)
        (let* ([finder_pattern_start_points (locate-finder-pattern modules)]
               [bottom_left_point (second finder_pattern_start_points)]
               [top_right_point (third finder_pattern_start_points)]
               [new_bottom_left_point (cons (- (car bottom_left_point) 4) (cdr bottom_left_point))]
               [new_top_right_point (cons (car top_right_point) (- (cdr top_right_point) 4))])
          
          (let loop ([data_list (string->list (hash-ref *version_code_hash* version))]
                     [trace_list (transform-points-list (first *trace_points*) new_bottom_left_point)])
            (when (and (not (null? data_list)) (not (null? trace_list)))
                  (if (char=? (car data_list) #\0)
                      (hash-set! points_map (car trace_list) '("0" . "format"))
                      (hash-set! points_map (car trace_list) '("1" . "format")))
                  (loop (cdr data_list) (cdr trace_list))))

          (let loop ([data_list (string->list (hash-ref *version_code_hash* version))]
                     [trace_list (transform-points-list (second *trace_points*) new_top_right_point)])
            (when (and (not (null? data_list)) (not (null? trace_list)))
                  (if (char=? (car data_list) #\0)
                      (hash-set! points_map (car trace_list) '("0" . "format"))
                      (hash-set! points_map (car trace_list) '("1" . "format")))
                  (loop (cdr data_list) (cdr trace_list)))))))
