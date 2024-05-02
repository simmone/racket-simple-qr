#lang racket

(require "../share/lib.rkt"
         "../share/qr.rkt"
         "finder-pattern.rkt")

(provide (contract-out
          [draw-version-information (-> QR? void?)]
          ))

(define (draw-version-information qr)
  (when (>= (QR-version qr) 7)
    (let* ([version_str (hash-ref (get-version-hash) (QR-version qr))]
           [finder_pattern_start_points (locate-finder-pattern (QR-modules qr))]
           [upper_right_point (second finder_pattern_start_points)]
           [lower_left_point (third finder_pattern_start_points)]
           [new_upper_right_point (cons (car upper_right_point) (- (cdr upper_right_point) 4))]
           [new_lower_left_point (cons (- (car lower_left_point) 4) (cdr lower_left_point))]
           [data_list (reverse (string->list version_str))]
           [lower_left_list (transform-points-list (second (get-version-points)) new_lower_left_point)]
           [upper_right_list (transform-points-list (first (get-version-points)) new_upper_right_point)])
    
      (let loop ([loop_list data_list]
                 [trace_list lower_left_list])
        (when (and (not (null? loop_list)) (not (null? trace_list)))
          (if (char=? (car loop_list) #\0)
              (add-point (car trace_list) 0 'version qr)
              (add-point (car trace_list) 1 'version qr))
          (loop (cdr loop_list) (cdr trace_list))))

      (let loop ([loop_list data_list]
                 [trace_list upper_right_list])
        (when (and (not (null? loop_list)) (not (null? trace_list)))
          (if (char=? (car loop_list) #\0)
              (add-point (car trace_list) 0 'version qr)
              (add-point (car trace_list) 1 'version qr))
          (loop (cdr loop_list) (cdr trace_list)))))))

  '(
    ((0 . 0) (0 . 1) (0 . 2)
     (1 . 0) (1 . 1) (1 . 2)
     (2 . 0) (2 . 1) (2 . 2)
     (3 . 0) (3 . 1) (3 . 2)
     (4 . 0) (4 . 1) (4 . 2)
     (5 . 0) (5 . 1) (5 . 2))
    ((0 . 0) (1 . 0) (2 . 0)
     (0 . 1) (1 . 1) (2 . 1)
     (0 . 2) (1 . 2) (2 . 2)
     (0 . 3) (1 . 3) (2 . 3)
     (0 . 4) (1 . 4) (2 . 4)
     (0 . 5) (1 . 5) (2 . 5))
    ))

(define (get-version-hash)
  '#hash(
         (7 .  "000111110010010100")
         (8 .  "001000010110111100")
         (9 .  "001001101010011001")
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

