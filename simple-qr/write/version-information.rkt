#lang racket

(require "../share/lib.rkt"
         "../share/qr.rkt"
         "../share/finder-pattern.rkt"
         "../share/version-information.rkt")

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
