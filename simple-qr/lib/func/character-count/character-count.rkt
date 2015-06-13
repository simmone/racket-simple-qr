#lang racket

(provide (contract-out
          [get-character-count (-> exact-nonnegative-integer? string? exact-nonnegative-integer?)]
          ))

(define (get-character-count version mode)
  (cond
   [(and (>= version 1) (<= version 9))
    (cond
     [(string=? mode "N")
      10]
     [(string=? mode "A")
      9]
     [(string=? mode "B")
      8]
     [(string=? mode "K")
      8])]
   [(and (>= version 10) (<= version 26))
    (cond
     [(string=? mode "N")
      12]
     [(string=? mode "A")
      11]
     [(string=? mode "B")
      16]
     [(string=? mode "K")
      10])]
   [(and (>= version 27) (<= version 40))
    (cond
     [(string=? mode "N")
      14]
     [(string=? mode "A")
      13]
     [(string=? mode "B")
      16]
     [(string=? mode "K")
      12])]))
