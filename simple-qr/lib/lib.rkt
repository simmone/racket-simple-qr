#lang racket

(provide (contract-out
          [get-encoded-data-group-from-bit-string (-> string? string? string? list?)]
          [get-encoded-data-group (->* (string?) 
                                       (#:mode string? #:error_level string?)
                                       list?)]
          ))

(require "data-encoding/data-encoding.rkt")
(require "func/func.rkt")
(require "func/code-info/code-info-func.rkt")

(define (get-encoded-data-group-from-bit-string bit_data version error_level)
  (let ([decimal_list #f]
        [split_contract #f]
        [data_group #f]
        )

    (printf "bit_data=[~a]\n" bit_data)
    
    (set! decimal_list (split-bit-string-to-decimal bit_data))
    (printf "decimal_list=[~a]\n" decimal_list)
    
    (set! split_contract (get-group-width version error_level))
    (printf "split_contract=[~a]\n" split_contract)
    
    (set! data_group (split-decimal-list-on-contract decimal_list split_contract))
    (printf "data_group=[~a]\n" data_group)
    
    data_group))


(define (get-encoded-data-group data #:mode [mode "B"] #:error_level [error_level "H"])
  (let ([version #f]
        [encoded_data #f]
        [decimal_list #f]
        [split_contract #f]
        [data_group #f]
        )

    (set! version (get-version data mode error_level))
    (printf "version=[~a] mode=[~a] error_level=[~a]\n" version mode error_level)
    
    (set! bit_data (data-encode data #:mode mode #:error_level error_level))

    (get-encoded-data-group-from-bit-string bit_data version error_level)
    ))

