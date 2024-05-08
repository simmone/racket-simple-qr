#lang racket

(require rackunit/text-ui
         rackunit
         "../../write/mask-data.rkt")

(define test-mask-data
  (test-suite 
   "test-mask-data"

   (test-case
    "test-mask-func"

    (let ([data '(((2 . 3) . 1) ((3 . 4) . 0) ((4 . 5) . 1) ((6 . 7) . 0) ((7 . 7) . 1) ((2 . 4) . 0))])
      (check-equal? 
       (mask-func data 0)
       '(((2 . 3) . 1) ((3 . 4) . 0) ((4 . 5) . 1) ((6 . 7) . 0) ((7 . 7) . 0) ((2 . 4) . 1)))

      (check-equal? 
       (mask-func data 1)
       '(((2 . 3) . 0) ((3 . 4) . 0) ((4 . 5) . 0) ((6 . 7) . 1) ((7 . 7) . 1) ((2 . 4) . 1)))

      (check-equal? 
       (mask-func data 2)
       '(((2 . 3) . 0) ((3 . 4) . 0) ((4 . 5) . 1) ((6 . 7) . 0) ((7 . 7) . 1) ((2 . 4) . 0)))

      (check-equal? 
       (mask-func data 3)
       '(((2 . 3) . 1) ((3 . 4) . 0) ((4 . 5) . 0) ((6 . 7) . 0) ((7 . 7) . 1) ((2 . 4) . 1)))

      (check-equal? 
       (mask-func data 4)
       '(((2 . 3) . 0) ((3 . 4) . 1) ((4 . 5) . 1) ((6 . 7) . 0) ((7 . 7) . 1) ((2 . 4) . 1)))

      (check-equal? 
       (mask-func data 5)
       '(((2 . 3) . 0) ((3 . 4) . 1) ((4 . 5) . 1) ((6 . 7) . 1) ((7 . 7) . 1) ((2 . 4) . 0)))

      (check-equal? 
       (mask-func data 6)
       '(((2 . 3) . 0) ((3 . 4) . 1) ((4 . 5) . 0) ((6 . 7) . 1) ((7 . 7) . 0) ((2 . 4) . 1)))

      (check-equal? 
       (mask-func data 7)
       '(((2 . 3) . 1) ((3 . 4) . 0) ((4 . 5) . 1) ((6 . 7) . 0) ((7 . 7) . 1) ((2 . 4) . 1)))
      )

    (let ([data '(((21 . 21) . 0) ((21 . 20) . 0) ((20 . 21) . 0) ((20 . 20) . 1) 
                  ((19 . 21) . 0) ((19 . 20) . 0) ((18 . 21) . 0) ((18 . 20) . 0))])
      (check-equal? 
       (mask-func data 3)
       '(((21 . 21) . 1) ((21 . 20) . 0) ((20 . 21) . 0) ((20 . 20) . 1) 
         ((19 . 21) . 0) ((19 . 20) . 1) ((18 . 21) . 1) ((18 . 20) . 0)))
    )
    )

   (test-case
    "test-get-all-data-rows/cols"
    
    (check-equal?
     (get-all-data-rows 2)
     '(((4 . 4) (4 . 5))
       ((5 . 4) (5 . 5))))

    (check-equal?
     (get-all-data-cols 2)
       '(((4 . 4) (5 . 4))
         ((4 . 5) (5 . 5))))

    (check-equal?
     (get-all-data-rows 3)
     '(((4 . 4) (4 . 5) (4 . 6))
       ((5 . 4) (5 . 5) (5 . 6))
       ((6 . 4) (6 . 5) (6 . 6))))

    (check-equal?
     (get-all-data-cols 3)
       '(((4 . 4) (5 . 4) (6 . 4))
         ((4 . 5) (5 . 5) (6 . 5))
         ((4 . 6) (5 . 6) (6 . 6))))

    (check-equal?
     (get-all-data-rows 5)
     '(((4 . 4) (4 . 5) (4 . 6) (4 . 7) (4 . 8))
       ((5 . 4) (5 . 5) (5 . 6) (5 . 7) (5 . 8))
       ((6 . 4) (6 . 5) (6 . 6) (6 . 7) (6 . 8))
       ((7 . 4) (7 . 5) (7 . 6) (7 . 7) (7 . 8))
       ((8 . 4) (8 . 5) (8 . 6) (8 . 7) (8 . 8))))

    (check-equal?
     (get-all-data-cols 5)
       '(((4 . 4) (5 . 4) (6 . 4) (7 . 4) (8 . 4))
         ((4 . 5) (5 . 5) (6 . 5) (7 . 5) (8 . 5))
         ((4 . 6) (5 . 6) (6 . 6) (7 . 6) (8 . 6))
         ((4 . 7) (5 . 7) (6 . 7) (7 . 7) (8 . 7))
         ((4 . 8) (5 . 8) (6 . 8) (7 . 8) (8 . 8)))))

   (test-case
    "test-condition1"

    (check-equal? (mask-condition1 '(0 0)) 0)
    (check-equal? (mask-condition1 '(1 1)) 0)

    (check-equal? (mask-condition1 '(0 0 0 0)) 0)
    (check-equal? (mask-condition1 '(1 1 1 1)) 0)

    (check-equal? (mask-condition1 '(0 0 0 0 0)) 3)
    (check-equal? (mask-condition1 '(1 1 1 1 1)) 3)

    (check-equal? (mask-condition1 '(0 0 0 0 0 0)) 4)
    (check-equal? (mask-condition1 '(0 0 0 0 1 0)) 0)
    (check-equal? (mask-condition1 '(1 1 1 1 1 1)) 4)
    (check-equal? (mask-condition1 '(1 1 1 1 0 1)) 0)

    (check-equal? (mask-condition1 '(0 0 0 0 0 0 0 0 1)) 6)
    (check-equal? (mask-condition1 '(1 1 1 1 1 1 1 1 1 0 0 0 0)) 7)

    (check-equal? (mask-condition1 '(1 1 1 1 1 1 1 1 1 0 0 0 0 0)) 10)
    (check-equal? (mask-condition1 '(1 1 1 1 1 1 1 1 1 0 0 0 0 0 0)) 11)
    (check-equal? (mask-condition1 '(1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0)) 12)
    (check-equal? (mask-condition1 '(1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)) 13)
    (check-equal? (mask-condition1 '(1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0)) 14)
    )
   
   (test-case
    "test-on-condition2"
    
    (let ([points_map (make-hash)])
      (hash-set! points_map '(1 . 1) 1) (hash-set! points_map '(1 . 2) 1) (hash-set! points_map '(1 . 3) 1)
      (hash-set! points_map '(2 . 1) 1) (hash-set! points_map '(2 . 2) 1) (hash-set! points_map '(2 . 3) 1)
      (hash-set! points_map '(3 . 1) 1) (hash-set! points_map '(3 . 2) 1) (hash-set! points_map '(3 . 3) 1)

      (check-equal? (mask-on-condition2 points_map) 12)
      
      (hash-set! points_map '(3 . 3) 0)

      (check-equal? (mask-on-condition2 points_map) 9)

      (hash-set! points_map '(2 . 3) 0)

      (check-equal? (mask-on-condition2 points_map) 6)
      ))

   (test-case
    "test-condition3"

    (check-equal? (mask-condition3 '(1 0 1 1 1 0 1 0 0 0 0)) 40)
    (check-equal? (mask-condition3 '(1 0 1 0 1 1 1 0 1 0 0 0 0 1)) 40)
    (check-equal? (mask-condition3 '(1 0 1 0 1 1 1 0 1 0 0 1 0 1)) 0)

    (check-equal? (mask-condition3 '(0 0 0 0 1 0 1 1 1 0 1)) 40)
    (check-equal? (mask-condition3 '(1 0 0 0 0 1 0 1 1 1 0 1 0)) 40)
    (check-equal? (mask-condition3 '(1 0 0 0 0 1 0 1 1 1 0 0 0)) 0)
    )

   (test-case
    "test-on-condition4"
    
    (let ([points_map (make-hash)])
      (hash-set! points_map '(4 . 4) 1) (hash-set! points_map '(4 . 5) 1) (hash-set! points_map '(4 . 6) 1)
      (hash-set! points_map '(5 . 4) 1) (hash-set! points_map '(5 . 5) 1) (hash-set! points_map '(5 . 6) 1)
      (hash-set! points_map '(6 . 4) 1) (hash-set! points_map '(6 . 5) 1) (hash-set! points_map '(6 . 6) 1)

      (check-equal? (mask-on-condition4 3 points_map) 100)

      (hash-set! points_map '(4 . 4) 0)
      (check-equal? (mask-on-condition4 3 points_map) 70)

      (hash-set! points_map '(4 . 5) 0)
      (check-equal? (mask-on-condition4 3 points_map) 50)

      (hash-set! points_map '(4 . 6) 0)
      (check-equal? (mask-on-condition4 3 points_map) 30)

      (hash-set! points_map '(5 . 4) 0)
      (check-equal? (mask-on-condition4 3 points_map) 10)

      (hash-set! points_map '(5 . 5) 0)
      (check-equal? (mask-on-condition4 3 points_map) 10)

      (hash-set! points_map '(5 . 6) 0)
      (check-equal? (mask-on-condition4 3 points_map) 30)

      (hash-set! points_map '(6 . 4) 0)
      (check-equal? (mask-on-condition4 3 points_map) 50)

      (hash-set! points_map '(6 . 5) 0)
      (check-equal? (mask-on-condition4 3 points_map) 70)

      (hash-set! points_map '(6 . 6) 0)
      (check-equal? (mask-on-condition4 3 points_map) 100)
      ))

   (test-case
    "test-conditon-1"

    (let ([points_map (make-hash)])
      (for-each
       (lambda (rec)
         (for-each
          (lambda (point)
            (hash-set! points_map point 0))
          rec))
       (get-all-data-rows 21))
      
      (for-each
       (lambda (point)
         (hash-set! points_map point 1))
       (let loop-row ([rows
                   '(
                     (1 1 1 1 1 1 1 0 1 1 0 0 0 0 1 1 1 1 1 1 1)
                     (1 0 0 0 0 0 1 0 1 0 0 1 0 0 1 0 0 0 0 0 1)
                     (1 0 1 1 1 0 1 0 1 0 0 1 1 0 1 0 1 1 1 0 1)
                     (1 0 1 1 1 0 1 0 1 0 0 0 0 0 1 0 1 1 1 0 1)
                     (1 0 1 1 1 0 1 0 1 0 1 0 0 0 1 0 1 1 1 0 1)
                     (1 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 1)
                     (1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 1 1 1 1 1)
                     (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
                     (0 1 1 0 1 0 1 1 0 0 0 0 1 0 1 0 1 1 1 1 1)
                     (0 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 1 0 0 0 1)
                     (0 0 1 1 0 1 1 1 0 1 1 0 0 0 1 0 1 1 0 0 0)
                     (0 1 1 0 1 1 0 1 0 0 1 1 0 1 0 1 0 1 1 1 0)
                     (1 0 0 0 1 0 1 0 1 0 1 1 1 0 1 1 1 0 1 0 1)
                     (0 0 0 0 0 0 0 0 1 1 0 1 0 0 1 0 0 0 1 0 1)
                     (1 1 1 1 1 1 1 0 1 0 1 0 0 0 0 1 0 1 1 0 0)
                     (1 0 0 0 0 0 1 0 0 1 0 1 1 0 1 1 0 1 0 0 0)
                     (1 0 1 1 1 0 1 0 1 0 1 0 0 0 1 1 1 1 1 1 1)
                     (1 0 1 1 1 0 1 0 0 1 0 1 0 1 0 1 0 0 0 1 0)
                     (1 0 1 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1 0 0 1)
                     (1 0 0 0 0 0 1 0 1 0 1 1 0 1 0 0 0 1 0 1 1)
                     (1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 0 0 0 0 1)
                     )]
                  [row 4]
                  [row_result_list '()])
         (if (not (null? rows))
             (loop-row
              (cdr rows)
              (add1 row)
              `(
                ,@(let loop-col ([col 4]
                                 [col_result_list '()])
                    (if (<= col 24)
                        (if (= (list-ref (car rows) (- col 4)) 0)
                            (loop-col (add1 col) col_result_list)
                            (loop-col (add1 col) (cons (cons row col) col_result_list)))
                        (reverse col_result_list)))
                ,@row_result_list))
             (reverse row_result_list))))

      (check-equal? (mask-on-condition1 21 points_map) 180)

      (check-equal? (mask-on-condition2 points_map) 90)

      (check-equal? (mask-on-condition3 21 points_map) 80)

      (check-equal? (mask-on-condition4 21 points_map) 0)
      ))

  (test-case
    "test-conditon-2"

    (let ([points_map (make-hash)])
      (for-each
       (lambda (rec)
         (for-each
          (lambda (point)
            (hash-set! points_map point 0))
          rec))
       (get-all-data-rows 21))
      
      (for-each
       (lambda (point)
         (hash-set! points_map point 1))
       (let loop-row ([rows
                   '(
                     (1 1 1 1 1 1 1 0 0 0 0 1 0 0 1 1 1 1 1 1 1)
                     (1 0 0 0 0 0 1 0 1 1 0 0 1 0 1 0 0 0 0 0 1)
                     (1 0 1 1 1 0 1 0 0 1 0 1 1 0 1 0 1 1 1 0 1)
                     (1 0 1 1 1 0 1 0 1 1 1 1 1 0 1 0 1 1 1 0 1)
                     (1 0 1 1 1 0 1 0 1 1 0 1 0 0 1 0 1 1 1 0 1)
                     (1 0 0 0 0 0 1 0 0 1 0 0 1 0 1 0 0 0 0 0 1)
                     (1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 1 1 1 1 1)
                     (0 0 0 0 0 0 0 0 1 1 0 1 1 0 0 0 0 0 0 0 0)
                     (0 1 0 1 1 1 1 0 1 1 0 0 1 1 1 0 1 1 0 1 0)
                     (1 0 1 1 1 1 0 1 0 0 0 0 1 1 1 1 0 1 1 1 0)
                     (0 0 1 0 1 0 1 1 0 0 0 1 0 0 1 1 0 0 0 0 0)
                     (1 0 1 1 0 1 0 0 0 1 0 1 1 0 0 0 1 1 0 0 0)
                     (1 1 0 1 1 1 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1)
                     (0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1 0 1 0 0 0)
                     (1 1 1 1 1 1 1 0 0 1 1 0 0 1 1 0 0 1 1 1 1)
                     (1 0 0 0 0 0 1 0 1 0 1 0 0 1 0 0 1 0 1 1 1)
                     (1 0 1 1 1 0 1 0 1 1 0 1 0 0 1 0 0 0 1 1 1)
                     (1 0 1 1 1 0 1 0 1 0 1 1 1 0 0 0 1 0 1 0 0)
                     (1 0 1 1 1 0 1 0 0 1 0 1 1 0 1 0 0 0 0 1 1)
                     (1 0 0 0 0 0 1 0 1 1 1 0 1 1 1 1 0 0 1 1 0)
                     (1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 0 0 0 0 1 0)
                     )]
                  [row 4]
                  [row_result_list '()])
         (if (not (null? rows))
             (loop-row
              (cdr rows)
              (add1 row)
              `(
                ,@(let loop-col ([col 4]
                                 [col_result_list '()])
                    (if (<= col 24)
                        (if (= (list-ref (car rows) (- col 4)) 0)
                            (loop-col (add1 col) col_result_list)
                            (loop-col (add1 col) (cons (cons row col) col_result_list)))
                        (reverse col_result_list)))
                ,@row_result_list))
             (reverse row_result_list))))

      (check-equal? (mask-on-condition1 21 points_map) 171)

      (check-equal? (mask-on-condition2 points_map) 102)

      (check-equal? (mask-on-condition3 21 points_map) 80)

      (check-equal? (mask-on-condition4 21 points_map) 0)
      ))

   (test-case
    "test-condition4"

    (let ([points_map (make-hash)])

      (for-each
       (lambda (rec)
         (for-each
          (lambda (point)
            (hash-set! points_map point 0))
          rec))
       (get-all-data-rows 21))

      (for-each
       (lambda (point)
         (hash-set! points_map point 1))
       (let loop-row ([rows
                   '(
                     (1 1 1 1 1 1 1 0 0 0 1 0 0 0 1 1 1 1 1 1 1)
                     (1 0 0 0 0 0 1 0 1 1 0 1 0 0 1 0 0 0 0 0 1)
                     (1 0 1 1 1 0 1 0 1 0 0 1 0 0 1 0 1 1 1 0 1)
                     (1 0 1 1 1 0 1 0 0 0 0 1 1 0 1 0 1 1 1 0 1)
                     (1 0 1 1 1 0 1 0 0 0 0 1 1 0 1 0 1 1 1 0 1)
                     (1 0 0 0 0 0 1 0 0 1 0 1 0 0 1 0 0 0 0 0 1)
                     (1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 1 1 1 1 1)
                     (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
                     (0 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0)
                     (1 0 0 0 0 1 0 1 1 1 1 0 1 1 0 0 1 1 1 1 1)
                     (1 0 1 1 1 0 1 1 0 1 0 1 1 0 1 0 0 0 1 0 0)
                     (0 1 1 1 0 0 0 1 0 1 0 0 0 1 0 0 1 0 1 1 0)
                     (1 0 1 1 0 0 1 0 0 1 0 1 1 0 0 0 0 0 1 0 0)
                     (0 0 0 0 0 0 0 0 1 0 0 1 0 1 0 1 0 0 1 1 0)
                     (1 1 1 1 1 1 1 0 0 0 1 0 1 1 1 1 0 1 0 1 1)
                     (1 0 0 0 0 0 1 0 1 1 0 0 0 1 1 1 0 0 1 1 0)
                     (1 0 1 1 1 0 1 0 0 0 0 1 1 0 1 1 0 0 0 1 1)
                     (1 0 1 1 1 0 1 0 1 0 1 0 0 1 0 0 1 1 0 1 0)
                     (1 0 1 1 1 0 1 0 1 1 1 0 1 1 0 0 1 1 0 0 0)
                     (1 0 0 0 0 0 1 0 1 1 1 1 0 0 1 1 0 1 0 0 0)
                     (1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 0 0 1 1 0)
                     )]
                  [row 4]
                  [row_result_list '()])
         (if (not (null? rows))
             (loop-row
              (cdr rows)
              (add1 row)
              `(
                ,@(let loop-col ([col 4]
                                 [col_result_list '()])
                    (if (<= col 24)
                        (if (= (list-ref (car rows) (- col 4)) 0)
                            (loop-col (add1 col) col_result_list)
                            (loop-col (add1 col) (cons (cons row col) col_result_list)))
                        (reverse col_result_list)))
                ,@row_result_list))
             (reverse row_result_list))))
      (check-equal? (mask-on-condition4 21 points_map) 0)
      ))

   ))


(run-tests test-mask-data)

