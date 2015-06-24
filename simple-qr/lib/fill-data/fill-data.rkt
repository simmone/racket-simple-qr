#lang racket

(provide (contract-out 
          [snake-modules (-> exact-nonnegative-integer? list?)]
          ))

(define (snake-modules modules)
  (reverse
   (let ([points_sum_count (- (* modules modules) modules)])
     (let loop ([point (cons modules modules)]
                [current_move 'up_left]
                [result_list '()])

       (when (= (cdr point) 7)
             (set! point (cons (car point) (- (cdr point) 1))))

;       (if (= (remainder (sub1 (car point)) modules) 0)
;           (printf "\n~a " point)
;           (printf "~a " point))

       (if (= (length result_list) points_sum_count)
           result_list
           (let ([next_point #f]
                 [next_move #f])
             (cond
              [(equal? current_move 'up_left)
               (begin
                 (set! next_point (cons (car point) (sub1 (cdr point))))
                 (set! next_move 'up_right))]
              [(equal? current_move 'up_right)
               (begin
                 (set! next_point (cons (sub1 (car point)) (add1 (cdr point))))
                 (set! next_move 'up_left))]
              [(equal? current_move 'down_left)
               (begin
                 (set! next_point (cons (car point) (sub1 (cdr point))))
                 (set! next_move 'down_right))]
              [(equal? current_move 'down_right)
               (begin
                 (set! next_point (cons (add1 (car point)) (add1 (cdr point))))
                 (set! next_move 'down_left))])

             (if (not (in-range? next_point modules))
                 (cond
                  [(equal? current_move 'up_right)
                   (loop (cons 1 (sub1 (cdr point))) 'down_left (cons point result_list))]
                  [(equal? current_move 'down_right)
                   (loop (cons modules (sub1 (cdr point))) 'up_left (cons point result_list))])
                 (loop next_point next_move (cons point result_list)))))))))
   
   (define (in-range? point modules)
     (and (>= (car point) 1) (<= (car point) modules) (>= (cdr point) 1) (<= (cdr point) modules)))
