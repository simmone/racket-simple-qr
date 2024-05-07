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
    "test-conditon1-3"

    (let ([rows (get-all-data-rows 21)]
          [points_map (make-hash)])
      
      (for-each
       (lambda (rec)
         (for-each
          (lambda (point)
            (hash-set! points_map point 0))
          rec))
       rows)
      
      (for-each
       (lambda (point)
         (hash-set! points_map point 1))
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



       '((4  . 4) (4  . 5) (4  . 6) (4  . 7) (4  . 8) (4  . 9) (4  . 10)          (4  . 12) (4  . 13)                                         (4  . 18) (4  . 19) (4  . 20) (4  . 21) (4  . 22) (4  . 23) (4  . 24)
         (5  . 4)                                              (5  . 10)          (5  . 12)                     (5  . 15)                     (5  . 18)                                                   (5  . 24)
         (6  . 4)          (6  . 6) (6  . 7) (6  . 8)          (6  . 10)          (6  . 12)                     (6  . 15) (6  . 16)           (6  . 18)           (6  . 20) (6  . 21) (6  . 22)           (6  . 24)
         (7  . 4)          (7  . 6) (7  . 7) (7  . 8)          (7  . 10)          (7  . 12)                                                   (7  . 18)           (7  . 20) (7  . 21) (7  . 22)           (7  . 24)
         (8  . 4)          (8  . 6) (8  . 7) (8  . 8)          (8  . 10)          (8  . 12)           (8  . 14)                               (8  . 18)           (8  . 20) (8  . 21) (8  . 22)           (8  . 24)
         (9  . 4)                                              (9  . 10)                              (9  . 14)                               (9  . 18)                                                   (9  . 24)
         (10  . 4) (10  . 5) (10  . 6) (10  . 7) (10  . 8) (10  . 9) (10  . 10)          (10  . 12)           (10  . 14)           (10  . 16)           (10  . 18) (10  . 19) (10  . 20) (10  . 21) (10  . 22) (10  . 23) (10  . 24)
                                                                                         (11  . 12)
                   (12  . 5) (12  . 6)          (12  . 8)          (12  . 10) (12  . 11)                                        (12  . 16)           (12  . 18)           (12  . 20) (12  . 21) (12  . 22) (12  . 23) (12  . 24)
                   (13 .  5)                                                       (13 .  12) (13 . 13) (13 . 14) (13 . 15)                                         (13 . 20)                               (13 . 24)
                           (14 . 6) (14 . 7)          (14 . 9) (14 . 10) (14 . 11)          (14 . 13) (14 . 14)                               (14 . 18)           (14 . 20) (14 . 21)
                  (15 . 5) (15 . 6)          (15 . 8) (15 . 9)          (15 . 11)                    (15 . 14) (15 . 15)           (15 . 17)           (15 . 19)           (15 . 21) (15 . 22) (15 . 23)
         (16 . 4)                            (16 . 8)          (16 . 10)          (16 . 12)           (16 . 14) (16 . 15) (16 . 16)           (16 . 18) (16 . 19) (16 . 20)           (16 . 22)           (16 . 24)
                                                                                 (17 . 12) (17 . 13)           (17 . 15)                     (17 . 18)                               (17 . 22)           (17 . 24)
         (18 . 4) (18 . 5) (18 . 6) (18 . 7) (18 . 8) (18 . 9) (18 . 10)          (18 . 12)           (18 . 14)                                         (18 . 19)           (18 . 21) (18 . 22)
         (19 . 4)                                              (19 . 10)                   (19 . 13)           (19 . 15) (19 . 16)           (19 . 18) (19 . 19)           (19 . 21) 
         (20 . 4)          (20 . 6) (20 . 7) (20 . 8)          (20 . 10)          (20 . 12)           (20 . 14)                               (20 . 18) (20 . 19) (20 . 20) (20 . 21) (20 . 22) (20 . 23) (20 . 24)
         (21 . 4)          (21 . 6) (21 . 7) (21 . 8)          (21 . 10)                   (21 . 13)           (21 . 15)           (21 . 17)           (21 . 19)                               (21 . 23)
         (22 . 4)          (22 . 6) (22 . 7) (22 . 8)          (22 . 10)          (22 . 12)                               (22 . 16) (22 . 17) (22 . 18) (22 . 19)           (22 . 21)                     (22 . 24)
         (23 . 4)                                              (23 . 10)          (23 . 12)           (23 . 14) (23 . 15)           (23 . 17)                               (23 . 21)           (23 . 23) (23 . 24)
         (24 . 4) (24 . 5) (24 . 6) (24 . 7) (24 . 8) (24 . 9) (24 . 10)                                                 (24 . 16) (24 . 17) (24 . 18) (24 . 19)                                         (24 . 24)))
 
      (check-equal? (mask-on-condition1 21 points_map) 180)

      (check-equal? (mask-on-condition2 points_map) 90)

      (check-equal? (mask-on-condition3 21 points_map) 80)

      (printf "start condition4\n")
      (check-equal? (mask-on-condition4 21 points_map) 10)
      ))


   (test-case
    "test-condition4"

    (let ([rows (get-all-data-rows 21)]
          [points_map (make-hash)])
      
      (for-each
       (lambda (rec)
         (for-each
          (lambda (point)
            (hash-set! points_map point 0))
          rec))
       rows)
      
      (for-each
       (lambda (point)
         (hash-set! points_map point 1))
       '((4  . 4) (4  . 5) (4  . 6) (4  . 7) (4  . 8) (4  . 9) (4  . 10)          (4  . 12) (4  . 13)                                         (4  . 18) (4  . 19) (4  . 20) (4  . 21) (4  . 22) (4  . 23) (4  . 24)
         (5  . 4)                                              (5  . 10)          (5  . 12)                     (5  . 15)                     (5  . 18)                                                   (5  . 24)
         (6  . 4)          (6  . 6) (6  . 7) (6  . 8)          (6  . 10)          (6  . 12)                     (6  . 15) (6  . 16)           (6  . 18)           (6  . 20) (6  . 21) (6  . 22)           (6  . 24)
         (7  . 4)          (7  . 6) (7  . 7) (7  . 8)          (7  . 10)          (7  . 12)                                                   (7  . 18)           (7  . 20) (7  . 21) (7  . 22)           (7  . 24)
         (8  . 4)          (8  . 6) (8  . 7) (8  . 8)          (8  . 10)          (8  . 12)           (8  . 14)                               (8  . 18)           (8  . 20) (8  . 21) (8  . 22)           (8  . 24)
         (9  . 4)                                              (9  . 10)                              (9  . 14)                               (9  . 18)                                                   (9  . 24)
         (10  . 4) (10  . 5) (10  . 6) (10  . 7) (10  . 8) (10  . 9) (10  . 10)          (10  . 12)           (10  . 14)           (10  . 16)           (10  . 18) (10  . 19) (10  . 20) (10  . 21) (10  . 22) (10  . 23) (10  . 24)
                                                                                         (11  . 12)
                   (12  . 5) (12  . 6)          (12  . 8)          (12  . 10) (12  . 11)                                        (12  . 16)           (12  . 18)           (12  . 20) (12  . 21) (12  . 22) (12  . 23) (12  . 24)
                   (13 .  5)                                                       (13 .  12) (13 . 13) (13 . 14) (13 . 15)                                         (13 . 20)                               (13 . 24)
                           (14 . 6) (14 . 7)          (14 . 9) (14 . 10) (14 . 11)          (14 . 13) (14 . 14)                               (14 . 18)           (14 . 20) (14 . 21)
                  (15 . 5) (15 . 6)          (15 . 8) (15 . 9)          (15 . 11)                    (15 . 14) (15 . 15)           (15 . 17)           (15 . 19)           (15 . 21) (15 . 22) (15 . 23)
         (16 . 4)                            (16 . 8)          (16 . 10)          (16 . 12)           (16 . 14) (16 . 15) (16 . 16)           (16 . 18) (16 . 19) (16 . 20)           (16 . 22)           (16 . 24)
                                                                                 (17 . 12) (17 . 13)           (17 . 15)                     (17 . 18)                               (17 . 22)           (17 . 24)
         (18 . 4) (18 . 5) (18 . 6) (18 . 7) (18 . 8) (18 . 9) (18 . 10)          (18 . 12)           (18 . 14)                                         (18 . 19)           (18 . 21) (18 . 22)
         (19 . 4)                                              (19 . 10)                   (19 . 13)           (19 . 15) (19 . 16)           (19 . 18) (19 . 19)           (19 . 21) 
         (20 . 4)          (20 . 6) (20 . 7) (20 . 8)          (20 . 10)          (20 . 12)           (20 . 14)                               (20 . 18) (20 . 19) (20 . 20) (20 . 21) (20 . 22) (20 . 23) (20 . 24)
         (21 . 4)          (21 . 6) (21 . 7) (21 . 8)          (21 . 10)                   (21 . 13)           (21 . 15)           (21 . 17)           (21 . 19)                               (21 . 23)
         (22 . 4)          (22 . 6) (22 . 7) (22 . 8)          (22 . 10)          (22 . 12)                               (22 . 16) (22 . 17) (22 . 18) (22 . 19)           (22 . 21)                     (22 . 24)
         (23 . 4)                                              (23 . 10)          (23 . 12)           (23 . 14) (23 . 15)           (23 . 17)                               (23 . 21)           (23 . 23) (23 . 24)
         (24 . 4) (24 . 5) (24 . 6) (24 . 7) (24 . 8) (24 . 9) (24 . 10)                                                 (24 . 16) (24 . 17) (24 . 18) (24 . 19)                                         (24 . 24)))
 
      (check-equal? (mask-on-condition1 21 points_map) 180)

      (check-equal? (mask-on-condition2 points_map) 90)

      (check-equal? (mask-on-condition3 21 points_map) 80)

      (printf "start condition4\n")
      (check-equal? (mask-on-condition4 21 points_map) 10)
      ))

   ))


(run-tests test-mask-data)

