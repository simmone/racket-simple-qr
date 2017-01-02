#lang racket

(require rackunit/text-ui)
(require rackunit "lib.rkt")
(require matrix-rotate)

(require racket/runtime-path)
(define-runtime-path test_file "test.png")

(define test-lib
  (test-suite 
   "test-lib"
   
   ;module_width is 5

   (test-case
    "tests"

    (let ([points_list (pic->points test_file)])
      (check-equal? (length points_list) 165)
      (check-equal? (length (car points_list)) 165)

      (check-equal? (list-ref (list-ref points_list 0) 0) 765)
      (check-equal? (list-ref (list-ref points_list 0) 164) 765)

      (check-equal? (list-ref (list-ref points_list 30) 0) 765)
      (check-equal? (list-ref (list-ref points_list 30) 5) 765)
      (check-equal? (list-ref (list-ref points_list 30) 10) 765)
      (check-equal? (list-ref (list-ref points_list 30) 15) 765)
      (check-equal? (list-ref (list-ref points_list 30) 20) 0)
      (check-equal? (list-ref (list-ref points_list 30) 25) 765)
      (check-equal? (list-ref (list-ref points_list 30) 30) 0)
      (check-equal? (list-ref (list-ref points_list 30) 35) 0)
      (check-equal? (list-ref (list-ref points_list 30) 40) 0)
      (check-equal? (list-ref (list-ref points_list 30) 45) 765)
      (check-equal? (list-ref (list-ref points_list 30) 50) 0)
      (check-equal? (list-ref (list-ref points_list 30) 55) 765)
      
      (check-equal? (find-threshold points_list) 382)
    ))
   
   (test-case
    "test-points->bw"
    
    (let* ([points '((100 1 80 10) (200 1 300 10))]
           [bw_points (points->bw points 50)])
      
      (check-equal? (list-ref (list-ref bw_points 0) 0) 0)
      (check-equal? (list-ref (list-ref bw_points 0) 1) 1)
      (check-equal? (list-ref (list-ref bw_points 0) 2) 0)
      (check-equal? (list-ref (list-ref bw_points 0) 3) 1)

      (check-equal? (list-ref (list-ref bw_points 1) 0) 0)
      (check-equal? (list-ref (list-ref bw_points 1) 1) 1)
      (check-equal? (list-ref (list-ref bw_points 1) 2) 0)
      (check-equal? (list-ref (list-ref bw_points 1) 3) 1)
      ))
   
   (test-case
    "test-points-rotate"

    (let* ([points_list (pic->points test_file)]
           [bw_points (points->bw points_list 50)]
           [rotated_points (matrix-rotate bw_points 164)])
      (void)
;      (points->pic rotated_points "result.png")
      )
    )

   (test-case
    "test-guess-first-dark-width"

    (let ([test_points '(1 1)])
      (check-equal? (guess-first-dark-width test_points) 2))
    
    (let ([test_points '(0 0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)])
      (check-equal? (guess-first-dark-width test_points) 1))

    (let ([test_points '(0 0 1 1 1 1 1 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)])
      (check-equal? (guess-first-dark-width test_points) 6))

    (let ([test_points '(0 0)])
      (check-equal? (guess-first-dark-width test_points) 0))
    )
   
   (test-case
    "test-squash-points"
    
    (let ([test_points '(0 0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)])
      (check-equal? (squash-points test_points 1) '(0 0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1))

      (check-equal? (squash-points test_points 2) '(0 1 0 1 1 0 1 0 0 1 0 1 1 0 1))

      (check-equal? (squash-points test_points 3) '(0 1 0 1 0 1 0 1 0 1 0 1))

      (check-equal? (squash-points test_points 4) '(0 1 0 1))
      )

    (let ([test_points '(0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1)])
      (check-equal? (squash-points test_points 4) '(0 1 0 0 1))
    )

    (let ([test_points '(0 0 0 0 0 1 1 1 1 1  0 0 0 0 0 0 0 1 1 1)])
      (check-equal? (squash-points test_points 4) '(0 1 0 0 1))
    )
    )

   (test-case
    "test-guess-module-width"

    (let ([test_points '(1 1)])
      (check-equal? (guess-module-width test_points) #f))
    
    (let ([test_points '(0 0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)])
      (check-equal? (guess-module-width test_points) (list 1 2 12)))

    (let ([test_points '(0 0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0)])
      (check-equal? (guess-module-width test_points) #f))

    (let ([test_points '(0 0 1 1 0 0 1 1 1 1 1 1 0 0 1 1 0 1 1 0 0 1 1 1 1 1 1 0 0 1 1)])
      (check-equal? (guess-module-width test_points) (list 2 2 18)))
    )

   (test-case
    "test-squash-matrix"

    (let ([test_points '(
                         (0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1)
                         (0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1)
                         (0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1)
                         (0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1)
                         )
                       ])
      (check-equal? (squash-matrix test_points 4) '((0 1 0 0 1)))
      )
    )

   (test-case
    "test-try-to-get-matrix"

    (let ([matrix '(
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0)
                    (0 1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1 0)
                    (0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0)
                    (0 1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1 0)
                    (0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    )]
          [expected_matrix '(
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    )])
      
      (check-equal? (try-to-get-matrix matrix 3 1 11) expected_matrix))
    )

   (test-case
    "test-verify-matrix"

    (let ([matrix '(
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    )])
      
      (check-equal? (verify-matrix matrix) matrix))

    (let ([matrix '(
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 1 1 1 1 1 0 0 1 0 1 1 1 1 1 1 1)
                    )])
      
      (check-equal? (verify-matrix matrix) #f))
    )

   (test-case
    "test-find-pattern"

    (let ([matrix '(
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)
                    (1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1)
                    (1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1)
                    )])

      (check-equal? (guess-finder-center-from-start matrix 1 2 0) '(3 . 3))
      (check-equal? (guess-finder-center-from-start matrix 1 2 10) '(3 . 13))
      (check-equal? (guess-finder-center-from-start matrix 1 12 0) '(13 . 3))

      (let-values ([(finder1_point finder2_point finder3_point)
                    (find-pattern matrix)])
        (check-equal? finder1_point '(3 . 3))
        (check-equal? finder2_point '(3 . 13))
        (check-equal? finder3_point '(13 . 3))
        )))

;   (test-case
;    "test-qr-read"
; 
;    (parameterize
;     ([*trace_level* 1])
;     (qr-read "test.png")
;;     (qr-read "test1.jpg")
;    )
;    )

   ))

(run-tests test-lib)
