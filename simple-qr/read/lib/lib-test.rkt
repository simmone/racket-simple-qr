#lang racket

(require rackunit/text-ui)
(require rackunit "lib.rkt")
(require rackunit "../matrix-rotate/lib.rkt")

(require racket/runtime-path)
(define-runtime-path test_file "normal.png")

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
      (check-equal? (squash-points test_points 3) '(0 1 0 1))
      (check-equal? (squash-points test_points 4) '(0 1 0 1))
      (check-equal? (squash-points test_points 5) '(1 0 1))

      (check-equal? (squash-points '(0 0 0 0 0 0 0 0 0 0) 5) '(0 0))
      (check-equal? (squash-points '(0 0 0 0 0 0 0 0 0 0 0) 5) '(0 0))
      (check-equal? (squash-points '(0 0 0 0 0 0 0 0 0 0 0 0) 5) '(0 0))
      (check-equal? (squash-points '(0 0 0 0 0 0 0 0 0 0 0 0 0) 5) '(0 0 0))
      )

    (let ([test_points '(0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1)])
      (check-equal? (squash-points test_points 4) '(0 1 0 0 1))
    )

    (let ([test_points '(0 0 0 0 0 1 1 1 1 1  0 0 0 0 0 0 0 1 1 1)])
      (check-equal? (squash-points test_points 4) '(0 1 0 0 1))
    )

    (let ([test_points
           '(0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)])
      (check-equal? (squash-points test_points 5) '(0 0 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 0 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0)))

    (let ([test_points
           '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)])
      (check-equal? (squash-points test_points 5) '(0 0 1 1 1 0 1 1 0 0 1 0 0 0 0 0 0 0 1 1 0 0 1 0 1 0 1 0 0 1 0 0 0 0)))
    )

   (test-case
    "test-align-matrix"
    
    (let ([matrix 
           '((1 2 3)
             (1 2 3 4)
             (1)
             ())])
      (check-equal? (align-matrix matrix 0)
                    '((1 2 3 0)
                      (1 2 3 4)
                      (1 0 0 0)
                      (0 0 0 0)))))

   (test-case
    "test-guess-module-width"

    (let ([test_points '(1 1)])
      (check-equal? (guess-module-width #f test_points) #f))
    
    (let ([test_points '(0 0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1)])
      (check-equal? (guess-module-width #f test_points) (list 1 2)))

    (let ([test_points '(0 0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0)])
      (check-equal? (guess-module-width #f test_points) '(1 2)))

    (let ([test_points '(0 0 1 1 0 0 1 1 1 1 1 1 0 0 1 1 0 0 0 0 1 1 0 0 1 1 1 1 1 1 0 0 1 1 0 0)])
      (check-equal? (guess-module-width #f test_points) (list 2 2 20)))
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
      (check-equal? (squash-matrix test_points 4) '((1 0 0 1)))
      )
    )

   (test-case
    "test-point-distance"
    
    (let ([point_distance (point-distance '(0 . 0) '(1 . 1))])
      (check-true (>= point_distance 1))
      )
    )
   
   (test-case
    "test-check-center-points-valid"
    
    (let ([points_distance_map
           '#hash((("3-13" . "3-3") . 10) (("3-13" . "13-3") . 15.0) (("13-3" . "3-3") . 10) (("13-3" . "3-13") . 15.0) (("3-3" . "3-13") . 10) (("3-3" . "13-3") . 10))])
      (check-true (check-center-points-valid points_distance_map)))

    (let ([points_distance_map
           '#hash((("3-13" . "3-3") . 11) (("3-13" . "13-3") . 15.0) (("13-3" . "3-3") . 10) (("13-3" . "3-13") . 15.0) (("3-3" . "3-13") . 10) (("3-3" . "13-3") . 10))])
      (check-false (check-center-points-valid points_distance_map)))
    )
   
   (test-case
    "test-get-center-points"

    (let ([points_distance_map
           '#hash((("3-13" . "3-3") . 10) (("3-13" . "13-3") . 15.0) (("13-3" . "3-3") . 10) (("13-3" . "3-13") . 15.0) (("3-3" . "3-13") . 10) (("3-3" . "13-3") . 10))])
      (check-equal? (get-center-points points_distance_map) '((3 . 3) (3 . 13) (13 . 3))))

    (let ([points_distance_map
           '#hash(
                  (("3-3" . "1-10") . 10) (("3-3" . "5-10") . 10.0) (("1-10" . "5-10") . 15)
                  (("1-10" . "3-3") . 10) (("5-10" . "3-3") . 10.0) (("5-10" . "1-10") . 15))])
      (check-equal? (get-center-points points_distance_map) '((3 . 3) (1 . 10) (5 . 10))))

    (let ([points_distance_map
           '#hash(
                  (("3-3" . "4-10") . 10) (("3-3" . "11-1") . 10.0) (("4-10" . "11-1") . 15)
                  (("4-10" . "3-3") . 10) (("11-1" . "3-3") . 10.0) (("11-1" . "4-10") . 15))])
      (check-equal? (get-center-points points_distance_map) '((3 . 3) (4 . 10) (11 . 1))))

    (let ([points_distance_map
           '#hash(
                  (("3-3" . "5-2") . 10) (("3-3" . "1-1") . 10.0) (("5-2" . "1-1") . 15)
                  (("5-2" . "3-3") . 10) (("1-1" . "3-3") . 10.0) (("1-1" . "5-2") . 15))])
      (check-equal? (get-center-points points_distance_map) '((3 . 3) (5 . 2) (1 . 1))))

    (let ([points_distance_map
           '#hash(
                  (("3-3" . "1-1") . 10) (("3-3" . "2-10") . 10.0) (("2-10" . "1-1") . 15)
                  (("1-1" . "3-3") . 10) (("2-10" . "3-3") . 10.0) (("1-1" . "2-10") . 15))])
      (check-equal? (get-center-points points_distance_map) '((3 . 3) (1 . 1) (2 . 10))))
    )

   (test-case
    "test-find-pattern"

    (let ([matrix '(
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
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 0 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 0 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 0 0 1 0)
                    (0 1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1 0)
                    (0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0)
                    )])


      (let ([finder_points (find-pattern-center-points matrix)])
        (when finder_points
              (check-equal? (first (cdr finder_points)) '(3 . 4))
              (check-equal? (second (cdr finder_points)) '(3 . 14))
              (check-equal? (third (cdr finder_points)) '(13 . 4)))
        )))

   (test-case
    "test-calculate-rotate-ratio"
    
    (let ([point_a '(2 . 2)]
          [point_b '(2 . 4)]
          [radius 2])
      
      (check-equal? (calculate-rotate-ratio point_a point_b radius) 0))

    (let ([point_a '(2 . 2)]
          [point_b '(3 . 4)]
          [point_c '(4 . 4)]
          [radius 2])
      
      (check-equal? (calculate-rotate-ratio point_a point_b radius) (/ 1 16))
      (check-equal? (calculate-rotate-ratio point_a point_c radius) (/ 1 8))
      )

    (let ([point_a '(2 . 2)]
          [point_b '(4 . 3)]
          [point_c '(4 . 2)]
          [radius 2])
      
      (check-equal? (calculate-rotate-ratio point_a point_b radius) (/ 3 16))
      (check-equal? (calculate-rotate-ratio point_a point_c radius) (/ 4 16))
      )

    (let ([point_a '(2 . 2)]
          [point_b '(4 . 1)]
          [point_c '(4 . 0)]
          [radius 2])
      
      (check-equal? (calculate-rotate-ratio point_a point_b radius) (/ 5 16))
      (check-equal? (calculate-rotate-ratio point_a point_c radius) (/ 6 16))
      )

    (let ([point_a '(2 . 2)]
          [point_b '(3 . 0)]
          [point_c '(2 . 0)]
          [radius 2])
      
      (check-equal? (calculate-rotate-ratio point_a point_b radius) (/ 7 16))
      (check-equal? (calculate-rotate-ratio point_a point_c radius) (/ 8 16))
      )

    (let ([point_a '(2 . 2)]
          [point_b '(1 . 0)]
          [point_c '(0 . 0)]
          [radius 2])
      
      (check-equal? (calculate-rotate-ratio point_a point_b radius) (/ -7 16))
      (check-equal? (calculate-rotate-ratio point_a point_c radius) (/ -6 16))
      )

    (let ([point_a '(2 . 2)]
          [point_b '(0 . 1)]
          [point_c '(0 . 2)]
          [radius 2])
      
      (check-equal? (calculate-rotate-ratio point_a point_b radius) (/ -5 16))
      (check-equal? (calculate-rotate-ratio point_a point_c radius) (/ -4 16))
      )

    (let ([point_a '(2 . 2)]
          [point_b '(0 . 3)]
          [point_c '(0 . 4)]
          [radius 2])
      
      (check-equal? (calculate-rotate-ratio point_a point_b radius) (/ -3 16))
      (check-equal? (calculate-rotate-ratio point_a point_c radius) (/ -2 16))
      )

    (let ([point_a '(2 . 2)]
          [point_b '(1 . 4)]
          [point_c '(2 . 4)]
          [radius 2])
      
      (check-equal? (calculate-rotate-ratio point_a point_b radius) (/ -1 16))
      (check-equal? (calculate-rotate-ratio point_a point_c radius) (/ 0 16))
      )
    )
   
   (test-case
    "test-trim-matrix"
    
    (let ([matrix 
           '(
             (0 0 0 0 0 0 0)
             (0 0 0 0 0 0 0)
             (0 0 0 0 1 0 0)
             (0 0 0 0 0 0 0)
             (0 0 1 1 1 1 0)
             (0 0 0 0 0 0 0))])
      (check-equal? (trim-matrix matrix)
                    '((0 0 1 0)
                      (0 0 0 0)
                      (1 1 1 1)))))
   
   (test-case
    "test-qr-read"
 
    (parameterize
     ([*trace_level* 1])
;     (void)
;     (qr-read "normal.png")
     (qr-read "real.jpg")
    )
    )

   ))

(run-tests test-lib)
