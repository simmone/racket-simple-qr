#lang racket

(provide (contract-out
  [get-version (->
                exact-nonnegative-integer?
                string?
                string?
                exact-nonnegative-integer?)]
  ))


(define (get-version char_count mode error_level)
  0)
