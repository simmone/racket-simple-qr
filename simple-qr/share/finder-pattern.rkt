#lang racket

(provide (contract-out
          [get-finder-pattern (-> list?)]
          [locate-finder-pattern (-> natural? list?)]
          ))

