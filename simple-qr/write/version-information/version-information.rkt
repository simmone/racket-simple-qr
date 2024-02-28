#lang racket

(require "../func/func.rkt")
(require "../../../share/func.rkt")
(require "../../../share/version-information.rkt")

(provide (contract-out
          [draw-version-information (-> string? natural? hash? hash? void?)]
          ))

(define (draw-version-information version_str modules points_map type_map)
  (let* ([finder_pattern_start_points (locate-finder-pattern modules)]
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
                (add-point (car trace_list) "0" "version" points_map type_map)
                (add-point (car trace_list) "1" "version" points_map type_map))
            (loop (cdr loop_list) (cdr trace_list))))

    (let loop ([loop_list data_list]
               [trace_list upper_right_list])
      (when (and (not (null? loop_list)) (not (null? trace_list)))
            (if (char=? (car loop_list) #\0)
                (add-point (car trace_list) "0" "version" points_map type_map)
                (add-point (car trace_list) "1" "version" points_map type_map))
            (loop (cdr loop_list) (cdr trace_list))))))
