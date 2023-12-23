#lang racket

(provide (contract-out
  [get-version (->
                natural?
                string?
                string?
                natural?)]
  ))

(require "capacity-dic.rkt")

(define (get-version char_count mode error_level)
  (let loop ([loop_list (hash-ref *capacity_table* (string-append mode "-" error_level))])
    (if (not (null? loop_list))
        (if (<= char_count (cdar loop_list))
            (caar loop_list)
            (loop (cdr loop_list)))
        (error (string-append "no such version: mode[" mode "]error_level[" error_level "]char_count[" (number->string char_count) "]")))))
