#lang racket

(provide (contract-out
          [get-encoded-data-group-from-bit-string (-> string? exact-nonnegative-integer? string? list?)]
          [get-encoded-data-group (->* (string?) 
                                       (#:mode string? #:error_level string?)
                                       list?)]
          [interleave-data-group (-> list? list?)]
          ))

(require "data-encoding/data-encoding.rkt")
(require "func/func.rkt")
(require "func/code-info/code-info-func.rkt")
(require "error-correct-code/error-correct-code.rkt")

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

