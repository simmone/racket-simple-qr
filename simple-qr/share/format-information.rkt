#lang racket

(provide (contract-out
          [get-format-information (-> list?)]
          ))

(define (get-format-information)
  '(
    (                                                                (1 . 9)
                                                                     (2 . 9)
                                                                     (3 . 9)
                                                                     (4 . 9)
                                                                     (5 . 9)
                                                                     (6 . 9)

                                                                     (8 . 9)
     (9 . 9) (9 . 8) (9 . 6) (9 . 5) (9 . 4) (9 . 3)         (9 . 2) (9 . 1))

    ((9 . 8) (9 . 7) (9 . 6) (9 . 5) (9 . 4) (9 . 3) (9 . 2) (9 . 1))

    (                                                                (2 . 9)
                                                                     (3 . 9)
                                                                     (4 . 9)
                                                                     (5 . 9)
                                                                     (6 . 9)
                                                                     (7 . 9)
                                                                     (8 . 9))
    ))
