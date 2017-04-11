#lang racket

(provide (contract-out 
          [get-mode-indicator (-> string? string?)]
          [get-indicator-mode (-> string? (or/c string? #f))]
          ))

(define (get-mode-bit-table)
  '#hash(
         ("N" . "0001") 
         ("A" . "0010") 
         ("B" . "0100") 
         ("K" . "1000") 
         ("E" . "0111")
         ))

(define (get-bit-mode-table)
  (let ([data_map (make-hash)])
    (hash-for-each
     (get-mode-bit-table)
     (lambda (error code)
       (hash-set! data_map code error)))
    data_map))

(define (get-mode-indicator mode)
  (hash-ref (get-mode-bit-table) mode))

(define (get-indicator-mode bits4)
  (hash-ref (get-bit-mode-table) bits4 #f))

