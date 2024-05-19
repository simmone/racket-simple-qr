#lang racket

(require "write/qr-write.rkt")

(provide (contract-out
          [qr-write (->* (string? path-string?)
                         (
                          #:mode string?
                          #:error_level string?
                          #:module_width natural?
                          #:color (cons/c string? (or/c string? 'transparent))
                          #:output_type (or/c 'png 'svg)
                         )
                         any)]
          ))
