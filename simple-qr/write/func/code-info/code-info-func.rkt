#lang racket

(provide (contract-out
     [get-bits-width (-> exact-nonnegative-integer? string? exact-nonnegative-integer?)]
     [get-ec-count (-> exact-nonnegative-integer? string? exact-nonnegative-integer?)]
     ))

(require "code-info-dic.rkt")

(define (get-bits-width version error_level)
  (hash-ref *required_bits_table* (string-append (number->string version) "-" error_level)))

(define (get-ec-count version error_level)
  (hash-ref *required_ec_table* (string-append (number->string version) "-" error_level)))

