#lang racket

(require rackunit/text-ui)

(require rackunit "mask-data.rkt")

(require "../func/func.rkt")
(require racket/draw)

(define test-mask-data
  (test-suite 
   "test-mask-data"

   (test-case
    "test-mask-func"

    (let ([data '(((2 . 3) . "1") ((3 . 4) . "0") ((4 . 5) . "1") ((6 . 7) . "0") ((7 . 7) . "1") ((2 . 4) . "0"))])
      (check-equal? 
       (mask-func data 0)
       '(((2 . 3) . "1") ((3 . 4) . "0") ((4 . 5) . "1") ((6 . 7) . "0") ((7 . 7) . "0") ((2 . 4) . "1")))

      (check-equal? 
       (mask-func data 1)
       '(((2 . 3) . "0") ((3 . 4) . "0") ((4 . 5) . "0") ((6 . 7) . "1") ((7 . 7) . "1") ((2 . 4) . "1")))

      (check-equal? 
       (mask-func data 2)
       '(((2 . 3) . "0") ((3 . 4) . "0") ((4 . 5) . "1") ((6 . 7) . "0") ((7 . 7) . "1") ((2 . 4) . "0")))

      (check-equal? 
       (mask-func data 3)
       '(((2 . 3) . "1") ((3 . 4) . "0") ((4 . 5) . "0") ((6 . 7) . "0") ((7 . 7) . "1") ((2 . 4) . "1")))

      (check-equal? 
       (mask-func data 4)
       '(((2 . 3) . "0") ((3 . 4) . "1") ((4 . 5) . "1") ((6 . 7) . "0") ((7 . 7) . "1") ((2 . 4) . "1")))

      (check-equal? 
       (mask-func data 5)
       '(((2 . 3) . "0") ((3 . 4) . "1") ((4 . 5) . "1") ((6 . 7) . "1") ((7 . 7) . "1") ((2 . 4) . "0")))

      (check-equal? 
       (mask-func data 6)
       '(((2 . 3) . "0") ((3 . 4) . "1") ((4 . 5) . "0") ((6 . 7) . "1") ((7 . 7) . "0") ((2 . 4) . "1")))

      (check-equal? 
       (mask-func data 7)
       '(((2 . 3) . "1") ((3 . 4) . "0") ((4 . 5) . "1") ((6 . 7) . "0") ((7 . 7) . "1") ((2 . 4) . "1")))
      )

    (let ([data '(((21 . 21) . "0") ((21 . 20) . "0") ((20 . 21) . "0") ((20 . 20) . "1") 
                  ((19 . 21) . "0") ((19 . 20) . "0") ((18 . 21) . "0") ((18 . 20) . "0"))])
      (check-equal? 
       (mask-func data 1)
       '(((21 . 21) . "0") ((21 . 20) . "0") ((20 . 21) . "1") ((20 . 20) . "0") 
         ((19 . 21) . "0") ((19 . 20) . "0") ((18 . 21) . "1") ((18 . 20) . "1")))
    )
    )

   (test-case
    "test-split-matrix"
    
    (check-equal?
     (split-matrix 2)
     '(((1 . 1) (1 . 2))
       ((2 . 1) (2 . 2))
       ((1 . 1) (2 . 1))
       ((1 . 2) (2 . 2))))

    (check-equal?
     (split-matrix 3)
     '(((1 . 1) (1 . 2) (1 . 3))
       ((2 . 1) (2 . 2) (2 . 3))
       ((3 . 1) (3 . 2) (3 . 3))
       ((1 . 1) (2 . 1) (3 . 1))
       ((1 . 2) (2 . 2) (3 . 2))
       ((1 . 3) (2 . 3) (3 . 3))))

    (check-equal?
     (split-matrix 5)
     '(((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5))
       ((2 . 1) (2 . 2) (2 . 3) (2 . 4) (2 . 5))
       ((3 . 1) (3 . 2) (3 . 3) (3 . 4) (3 . 5))
       ((4 . 1) (4 . 2) (4 . 3) (4 . 4) (4 . 5))
       ((5 . 1) (5 . 2) (5 . 3) (5 . 4) (5 . 5))
       ((1 . 1) (2 . 1) (3 . 1) (4 . 1) (5 . 1))
       ((1 . 2) (2 . 2) (3 . 2) (4 . 2) (5 . 2))
       ((1 . 3) (2 . 3) (3 . 3) (4 . 3) (5 . 3))
       ((1 . 4) (2 . 4) (3 . 4) (4 . 4) (5 . 4))
       ((1 . 5) (2 . 5) (3 . 5) (4 . 5) (5 . 5))))
    )

   (test-case
    "test-condition1"

    (check-equal? (mask-condition1 '("0" "0")) 0)
    (check-equal? (mask-condition1 '("1" "1")) 0)

    (check-equal? (mask-condition1 '("0" "0" "0" "0")) 0)
    (check-equal? (mask-condition1 '("1" "1" "1" "1")) 0)

    (check-equal? (mask-condition1 '("0" "0" "0" "0" "0")) 3)
    (check-equal? (mask-condition1 '("1" "1" "1" "1" "1")) 3)

    (check-equal? (mask-condition1 '("0" "0" "0" "0" "0" "0")) 4)
    (check-equal? (mask-condition1 '("0" "0" "0" "0" "1" "0")) 0)
    (check-equal? (mask-condition1 '("1" "1" "1" "1" "1" "1")) 4)
    (check-equal? (mask-condition1 '("1" "1" "1" "1" "0" "1")) 0)

    (check-equal? (mask-condition1 '("0" "0" "0" "0" "0" "0" "0" "0" "1")) 6)
    (check-equal? (mask-condition1 '("1" "1" "1" "1" "1" "1" "1" "1" "1" "0" "0" "0" "0")) 7)

    (check-equal? (mask-condition1 '("1" "1" "1" "1" "1" "1" "1" "1" "1" "0" "0" "0" "0" "0")) 10)
    (check-equal? (mask-condition1 '("1" "1" "1" "1" "1" "1" "1" "1" "1" "0" "0" "0" "0" "0" "0")) 11)
    (check-equal? (mask-condition1 '("1" "1" "1" "1" "1" "1" "1" "1" "1" "0" "0" "0" "0" "0" "0" "0")) 12)
    (check-equal? (mask-condition1 '("1" "1" "1" "1" "1" "1" "1" "1" "1" "0" "0" "0" "0" "0" "0" "0" "0")) 13)
    (check-equal? (mask-condition1 '("1" "1" "1" "1" "1" "1" "1" "1" "1" "0" "0" "0" "0" "0" "0" "0" "0" "0")) 14)
    )
   
   (test-case
    "test-on-condition2"
    
    (let ([points_map (make-hash)])
      (hash-set! points_map '(1 . 1) "1") (hash-set! points_map '(1 . 2) "1") (hash-set! points_map '(1 . 3) "1")
      (hash-set! points_map '(2 . 1) "1") (hash-set! points_map '(2 . 2) "1") (hash-set! points_map '(2 . 3) "1")
      (hash-set! points_map '(3 . 1) "1") (hash-set! points_map '(3 . 2) "1") (hash-set! points_map '(3 . 3) "1")

      (check-equal? (mask-on-condition2 points_map) 12)
      
      (hash-set! points_map '(3 . 3) "0")

      (check-equal? (mask-on-condition2 points_map) 9)

      (hash-set! points_map '(2 . 3) "0")

      (check-equal? (mask-on-condition2 points_map) 6)
      ))

   (test-case
    "test-condition3"

    (check-equal? (mask-condition3 '("1" "0" "1" "1" "1" "0" "1" "0" "0" "0" "0")) 40)
    (check-equal? (mask-condition3 '("1" "0" "1" "0" "1" "1" "1" "0" "1" "0" "0" "0" "0" "1")) 40)
    (check-equal? (mask-condition3 '("1" "0" "1" "0" "1" "1" "1" "0" "1" "0" "0" "1" "0" "1")) 0)

    (check-equal? (mask-condition3 '("0" "0" "0" "0" "1" "0" "1" "1" "1" "0" "1")) 40)
    (check-equal? (mask-condition3 '("1" "0" "0" "0" "0" "1" "0" "1" "1" "1" "0" "1" "0")) 40)
    (check-equal? (mask-condition3 '("1" "0" "0" "0" "0" "1" "0" "1" "1" "1" "0" "0" "0")) 0)
    )

   (test-case
    "test-on-condition4"
    
    (let ([points_map (make-hash)])
      (hash-set! points_map '(1 . 1) "1") (hash-set! points_map '(1 . 2) "1") (hash-set! points_map '(1 . 3) "1")
      (hash-set! points_map '(2 . 1) "1") (hash-set! points_map '(2 . 2) "1") (hash-set! points_map '(2 . 3) "1")
      (hash-set! points_map '(3 . 1) "1") (hash-set! points_map '(3 . 2) "1") (hash-set! points_map '(3 . 3) "1")

      (check-equal? (mask-on-condition4 points_map) 100)

      (hash-set! points_map '(1 . 1) "0")
      (check-equal? (mask-on-condition4 points_map) 70)

      (hash-set! points_map '(1 . 2) "0")
      (check-equal? (mask-on-condition4 points_map) 50)

      (hash-set! points_map '(1 . 3) "0")
      (check-equal? (mask-on-condition4 points_map) 30)

      (hash-set! points_map '(2 . 1) "0")
      (check-equal? (mask-on-condition4 points_map) 10)

      (hash-set! points_map '(2 . 2) "0")
      (check-equal? (mask-on-condition4 points_map) 10)

      (hash-set! points_map '(2 . 3) "0")
      (check-equal? (mask-on-condition4 points_map) 30)

      (hash-set! points_map '(3 . 1) "0")
      (check-equal? (mask-on-condition4 points_map) 50)

      (hash-set! points_map '(3 . 2) "0")
      (check-equal? (mask-on-condition4 points_map) 70)

      (hash-set! points_map '(3 . 3) "0")
      (check-equal? (mask-on-condition4 points_map) 100)
      ))

   (test-case
    "test-evaluation"

    (let ([matrix (split-matrix 21)]
          [points_map (make-hash)])
      
      (for-each
       (lambda (rec)
         (for-each
          (lambda (point)
            (hash-set! points_map point "0"))
          rec))
       matrix)
      
      (for-each
       (lambda (point)
         (hash-set! points_map point "1"))
       '((1  . 1) (1  . 2) (1  . 3) (1  . 4) (1  . 5) (1  . 6) (1  . 7)          (1  . 9) (1  . 10)                                         (1  . 15) (1  . 16) (1  . 17) (1  . 18) (1  . 19) (1  . 20) (1  . 21)
         (2  . 1)                                              (2  . 7)          (2  . 9)                     (2  . 12)                     (2 . 15)                                                    (2  . 21)
         (3  . 1)          (3  . 3) (3  . 4) (3  . 5)          (3  . 7)          (3  . 9)                     (3  . 12) (3  . 13)           (3  . 15)           (3  . 17) (3  . 18) (3  . 19)           (3  . 21)
         (4  . 1)          (4  . 3) (4  . 4) (4  . 5)          (4  . 7)          (4  . 9)                                                   (4  . 15)           (4  . 17) (4  . 18) (4  . 19)           (4  . 21)
         (5  . 1)          (5  . 3) (5  . 4) (5  . 5)          (5  . 7)          (5  . 9)           (5  . 11)                               (5  . 15)           (5  . 17) (5  . 18) (5  . 19)           (5  . 21)
         (6  . 1)                                              (6  . 7)                             (6  . 11)                               (6  . 15)                                                   (6  . 21)
         (7  . 1) (7  . 2) (7  . 3) (7  . 4) (7  . 5) (7  . 6) (7  . 7)          (7  . 9)           (7  . 11)           (7  . 13)           (7  . 15) (7  . 16) (7  . 17) (7  . 18) (7  . 19) (7  . 20) (7  . 21)
                                                                                 (8  . 9)
                  (9  . 2) (9  . 3)          (9  . 5)          (9  . 7) (9  . 8)                                        (9  . 13)           (9  . 15)           (9  . 17) (9  . 18) (9  . 19) (9  . 20) (9  . 21)
                  (10 . 2)                                                       (10 . 9) (10 . 10) (10 . 11) (10 . 12)                                         (10 . 17)                               (10 . 21)
                           (11 . 3) (11 . 4)          (11 . 6) (11 . 7) (11 . 8)          (11 . 10) (11 . 11)                               (11 . 15)           (11 . 17) (11 . 18)
                  (12 . 2) (12 . 3)          (12 . 5) (12 . 6)          (12 . 8)                    (12 . 11) (12 . 12)           (12 . 14)           (12 . 16)           (12 . 18) (12 . 19) (12 . 20)
         (13 . 1)                            (13 . 5)          (13 . 7)          (13 . 9)           (13 . 11) (13 . 12) (13 . 13)           (13 . 15) (13 . 16) (13 . 17)           (13 . 19)           (13 . 21)
                                                                                 (14 . 9) (14 . 10)           (14 . 12)                     (14 . 15)                               (14 . 19)           (14 . 21)
         (15 . 1) (15 . 2) (15 . 3) (15 . 4) (15 . 5) (15 . 6) (15 . 7)          (15 . 9)           (15 . 11)                                         (15 . 16)           (15 . 18) (15 . 19)
         (16 . 1)                                              (16 . 7)                   (16 . 10)           (16 . 12) (16 . 13)           (16 . 15) (16 . 16)           (16 . 18) 
         (17 . 1)          (17 . 3) (17 . 4) (17 . 5)          (17 . 7)          (17 . 9)           (17 . 11)                               (17 . 15) (17 . 16) (17 . 17) (17 . 18) (17 . 19) (17 . 20) (17 . 21)
         (18 . 1)          (18 . 3) (18 . 4) (18 . 5)          (18 . 7)                   (18 . 10)           (18 . 12)           (18 . 14)           (18 . 16)                               (18 . 20)
         (19 . 1)          (19 . 3) (19 . 4) (19 . 5)          (19 . 7)          (19 . 9)                               (19 . 13) (19 . 14) (19 . 15) (19 . 16)           (19 . 18)                     (19 . 21)
         (20 . 1)                                              (20 . 7)          (20 . 9)           (20 . 11) (20 . 12)           (20 . 14)                               (20 . 18)           (20 . 20) (20 . 21)
         (21 . 1) (21 . 2) (21 . 3) (21 . 4) (21 . 5) (21 . 6) (21 . 7)                                                 (21 . 13) (21 . 14) (21 . 15) (21 . 16)                                         (21 . 21)))

;      (let* ([modules 21]
;             [module_width 30]
;             [canvas_width (* modules module_width)]
;             [target (make-bitmap canvas_width canvas_width)]
;             [dc (new bitmap-dc% [bitmap target])])
;        (draw-debug-points dc module_width points_map)
;
;        (send target save-file "test.png" 'png)
;      
;        (system "open test.png")
;        )
      
      (check-equal? (mask-on-condition1 21 points_map) 180)

      (check-equal? (mask-on-condition2 points_map) 90)

      (check-equal? (mask-on-condition3 21 points_map) 80)

      (check-equal? (mask-on-condition4 points_map) 0)
      ))
   ))


(run-tests test-mask-data)

