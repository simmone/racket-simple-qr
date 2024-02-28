#lang racket

(provide (contract-out
          [struct POINT
                  (
                   (x natural?)
                   (y natural?)
                   )
                  ]
          ))

(struct POINT
        (
         (x #:mutable)
         (y #:mutable)
         )
        #:transparent
        )
