#lang racket

(provide (contract-out
          [matrix-data (->* (string?) (#:mode string? #:error_level string?) string?)]
          [get-encoded-data-group-from-bit-string (-> string? exact-nonnegative-integer? string? list?)]
          [get-encoded-data-group (->* (string?) 
                                       (#:mode string? #:error_level string?)
                                       list?)]
          [interleave-data-group (-> list? list?)]
          ))

(require racket/format)

(require "func/func.rkt")
(require "func/code-info/code-info-func.rkt")
(require "func/remainder-bits/remainder-bits-func.rkt")
(require "data-encoding/data-encoding.rkt")
(require "error-correct-code/error-correct-code.rkt")

(define (matrix-data data #:mode [mode "B"] #:error_level [error_level "H"])
  (let ([version #f]
        [data_grouped #f]
        [interleave_data_group #f]
        [padded_remainder_bits #f]
        )

    (set! version (get-version data mode error_level))
    ; (printf "st1: version=[~a]\n" version)

    (set! data_grouped (get-encoded-data-group data #:mode mode #:error_level error_level))
    ; (printf "st2: data_grouped=[~a]\n" data_grouped)
    
    (set! interleave_data_group (decimal-list-to-string (interleave-data-group data_grouped)))
    ; (printf "st3: interleave_data_group=[~a]\n" interleave_data_group)
    
    (set! padded_remainder_bits (~a interleave_data_group #:min-width (+ (string-length interleave_data_group) (get-remainder-bits version)) #:right-pad-string "0"))
    ; (printf "st4: padded_remainder_bits=[~a]\n" padded_remainder_bits)
    
    padded_remainder_bits))

(define (get-encoded-data-group-from-bit-string bit_data version error_level)
  (let ([encoded_data #f]
        [decimal_list #f]
        [split_contract #f]
        [origin_data_group #f]
        [data_group #f]
        )

    ; (printf "bit_data=[~a]\n" bit_data)
    
    (set! decimal_list (split-bit-string-to-decimal bit_data))
    ; (printf "decimal_list=[~a]\n" decimal_list)
    
    (set! split_contract (get-group-width version error_level))
    ; (printf "split_contract=[~a]\n" split_contract)
    
    (set! origin_data_group (split-decimal-list-on-contract decimal_list split_contract))
    ; (printf "data_group=[~a]\n" origin_data_group)
    
    (list
     (map
      (lambda (block_list)
        (list block_list
              (error-code block_list version error_level)))
      (car origin_data_group))

     (map
      (lambda (block_list)
        (list block_list
              (error-code block_list version error_level)))
      (cadr origin_data_group)))))

(define (get-encoded-data-group data #:mode [mode "B"] #:error_level [error_level "H"])
  (let ([version #f]
        [bit_data #f]
        )

    (set! version (get-version data mode error_level))
    ; (printf "version=[~a] mode=[~a] error_level=[~a]\n" version mode error_level)
    
    (set! bit_data (data-encode data #:mode mode #:error_level error_level))

    (get-encoded-data-group-from-bit-string bit_data version error_level)
    ))

(define (interleave-data-group data_group)
  (let ([data_list
         (quasiquote
          (
           (unquote-splicing (map car (car data_group)))
           (unquote-splicing (map car (cadr data_group)))))]
        [ec_list
         (quasiquote
          (
          (unquote-splicing (map cadr (car data_group)))
          (unquote-splicing (map cadr (cadr data_group)))))])
    `(,@(interleave-list data_list) ,@(interleave-list ec_list))))

