#lang racket

(provide (contract-out 
          [get-version (-> string? string? string? exact-nonnegative-integer?)]
          [get-mode-indicator (-> string? string?)]
          [get-character-count-indicator (-> exact-nonnegative-integer? exact-nonnegative-integer? string? string?)]
          ))

(require "../func/global.rkt")
(require "../func/capacity/capacity-func.rkt")
(require "../func/character-count/character-bit-width.rkt")

(require racket/format)

(define (get-version content mode error_level)
  (get-version-origin (string-length content) mode error_level))

(define (get-mode-indicator mode)
  (hash-ref *mode_bit_table* mode))

(define (get-character-count-indicator character_count version mode)
  (~r character_count #:base 2 #:min-width (get-character-bit-width version mode) #:pad-string "0"))
  
