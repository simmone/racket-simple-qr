#lang racket

(require rackunit/text-ui
         rackunit
         "../../share/lib.rkt")

(define test-func
  (test-suite 
   "test-func"

   (test-case
    "test-transform-points"

    (let* ([points_list
            '((1 . 2) (3 . 4) (4 . 3))]
           [transformed_points_list #f])
      
      (set! transformed_points_list
            (transform-points-list points_list '(0 . 0)))
      (check-equal? transformed_points_list '((1 . 2) (3 . 4) (4 . 3)))

      (set! transformed_points_list
            (transform-points-list points_list '(0 . 1)))
      (check-equal? transformed_points_list '((1 . 3) (3 . 5) (4 . 4)))

      ))

   (test-case 
    "test-locate-brick"
    
    (let ([place_pair (locate-brick 1 (cons 0 0))])
      (check-equal? place_pair '(0 . 0)))

    (let ([place_pair (locate-brick 2 (cons 0 0))])
      (check-equal? place_pair '(0 . 0)))

    (let ([place_pair (locate-brick 2 (cons 1 1))])
      (check-equal? place_pair '(2 . 2)))

    (let ([place_pair (locate-brick 2 (cons 4 4))])
      (check-equal? place_pair '(8 . 8)))

    (let ([place_pair (locate-brick 3 (cons 4 4))])
      (check-equal? place_pair '(12 . 12)))

    (let ([place_pair (locate-brick 3 (cons 4 6))])
      (check-equal? place_pair '(18 . 12)))
    )

   (test-case
    "test-hex_color->racket_color"

    (let ([racket_color (hex_color->racket_color "#9933cc")])
      (check-equal? (send racket_color red) 153)
      (check-equal? (send racket_color green) 51)
      (check-equal? (send racket_color blue) 204))

    (let ([racket_color (hex_color->racket_color "red")])
      (check-equal? racket_color "red"))
    )

   (test-case
    "test-get-points-between"

    (check-equal?
     (get-points-between '(1 . 1) '(1 . 10) #:direction 'horizontal)
     '((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6) (1 . 7) (1 . 8) (1 . 9) (1 . 10)))

    (check-equal?
     (get-points-between '(1 . 1) '(1 . 10) #:direction 'vertical)
     '())

    (check-equal?
     (get-points-between '(1 . 1) '(5 . 1) #:direction 'vertical)
     '((1 . 1) (2 . 1) (3 . 1) (4 . 1) (5 . 1)))

    (check-equal?
     (get-points-between '(1 . 1) '(5 . 1) #:direction 'horizontal)
     '())

    (check-equal?
     (get-points-between '(1 . 1) '(5 . 2) #:direction 'vertical)
     '())
    )
   
   (test-case
    "test-get-points"
    
    (let ([matrix '(
                    (0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0)
                    (0 1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 1 0)
                    (0 1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 1 0)
                    (0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0)
                    )])
      
      (check-equal? (get-points matrix '((6 . 7) (5 . 6) (4 . 5))) '(0 1 1))
      ))

   ))

(run-tests test-func)
