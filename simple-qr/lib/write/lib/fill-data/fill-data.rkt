#lang racket

(provide (contract-out 
          [draw-data (-> list? list? hash? hash? void?)]
          ))

(require "../func/func.rkt")

(define (draw-data data_list trace_list points_map type_map)
  (let loop ([loop_data_list data_list]
             [loop_trace_list trace_list])
    (when (not (null? loop_trace_list))
          (let ([bit_data (if (null? loop_data_list) #\0 (car loop_data_list))]
                [point_pair (car loop_trace_list)])
            (if (char=? bit_data #\1)
                (add-point point_pair "1" "data" points_map type_map)
                (add-point point_pair "0" "data" points_map type_map)))
          (if (null? loop_data_list)
              (loop '() (cdr loop_trace_list))
              (loop (cdr loop_data_list) (cdr loop_trace_list))))))
