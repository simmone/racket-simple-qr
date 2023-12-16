#lang racket

(require "write/qr-write.rkt")
(require "read/qr-read.rkt")

(provide (contract-out
          [qr-write (->* (string? path-string?) 
                         (
                          #:mode string?
                          #:error_level string?
                          #:module_width exact-nonnegative-integer?
                          #:color (cons/c string? string?)
                          #:output_type (or/c 'png 'svg)
                          )
                         any)]
          [qr-read (-> path-string? string?)]
          ))
