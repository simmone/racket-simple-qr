#lang racket

(require racket/draw)

(require "func.rkt")

(provide (contract-out
          [locate-timing-pattern-joints (->
                                         exact-nonnegative-integer?
                                         list?)]
          [draw-timing-pattern (->
                                (is-a?/c bitmap-dc%)
                                exact-nonnegative-integer?
                                exact-nonnegative-integer?
                                void?)]
                                
          ))

(define (locate-timing-pattern-joints modules)
  (let ([joint (+ 9 (- modules 16 1))])
    (list (cons (cons 9  7) (cons joint 7))
          (cons (cons 7  9) (cons 7 joint)))))

(define (draw-timing-pattern dc modules module_width)
  (let* ([joints (locate-timing-pattern-joints modules)]
         [horizontal_joints (car joints)]
         [vertical_joints (cdr joints)]
         [horizontal_points (get-points-between (car horizontal_joints) (cdr horizontal_joints) #:direction 'horizontal)]
         [vertical_points (get-points-between (car vertical_joints) (cdr vertical_joints) #:direction 'vertical)])

    (printf "~a\n" modules)
    (printf "~a\n" joints)
    (printf "~a\n" vertical_joints)
    
    (let loop ([points vertical_joints])
      (when (not (null? points))
            (let ([point (car points)])
              (printf "~a\n" point)
              (if (= (remainder (car point) 2) 1)
                  (draw-module dc "black" point module_width)
                  (draw-module dc "white" point module_width)))
            (loop (cdr points))))

    (let loop ([points horizontal_joints])
      (when (not (null? points))
            (let ([point (car points)])
              (if (= (remainder (cdr point) 2) 1)
                  (draw-module dc "black" point module_width)
                  (draw-module dc "white" point module_width)))
            (loop (cdr points))))))
    
    
                 

