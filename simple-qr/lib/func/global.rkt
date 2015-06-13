#lang racket

(provide (contract-out
          [*mode_bit_table* hash?]
          ))

(define *mode_bit_table* '#hash(("N" . "0001") ("A" . "0010") ("B" . "0100") ("K" . "1000") ("E" . "0111")))
