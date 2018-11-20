#lang racket

(require "header/header.rkt")
(require "input/input.rkt")
(require "origin-bits/origin-bits.rkt")
(require "bw-bits/bw-bits.rkt")
(require "finder-pattern-center-points/finder-pattern-center-points.rkt")
(require "rotate-ratio/rotate-ratio.rkt")
(require "rotated-bits/rotated-bits.rkt")
(require "trimed-bits/trimed-bits.rkt")
(require "final-bits/final-bits.rkt")
(require "basic-information/basic-information.rkt")
(require "exclude-finder-pattern/exclude-finder-pattern.rkt")
(require "exclude-separator/exclude-separator.rkt")
(require "exclude-timing-pattern/exclude-timing-pattern.rkt")
(require "exclude-alignment-pattern/exclude-alignment-pattern.rkt")
(require "exclude-format-information/exclude-format-information.rkt")
(require "exclude-version-information/exclude-version-information.rkt")
(require "exclude-dark-module/exclude-dark-module.rkt")
(require "unmask-data/unmask-data.rkt")
(require "ungroup-data/ungroup-data.rkt")
(require "decoded-data/decoded-data.rkt")
(require "final-string/final-string.rkt")

(provide (contract-out
          [write-report-header (-> path-string? void?)]
          [write-report-input (-> path-string? void?)]
          [write-report-origin-bits (-> natural? natural? list? path-string? void?)]
          [write-report-bw-bits (-> natural? natural? list? path-string? void?)]
          [write-report-finder-pattern-center-points (-> natural? list? natural? list? path-string? void?)]
          [write-report-rotate-ratio (-> number? path-string? void?)]
          [write-report-rotated-bits (-> natural? list? path-string? void?)]
          [write-report-trimed-bits (-> natural? list? path-string? void?)]
          [write-report-final-bits (-> natural? list? path-string? void?)]
          [write-report-basic-information (-> natural? string? string? string? string? path-string? void?)]
          [write-report-exclude-finder-pattern (-> natural? hash? hash? path-string? void?)]
          [write-report-exclude-separator (-> natural? hash? hash? path-string? void?)]
          [write-report-exclude-timing-pattern (-> natural? hash? hash? path-string? void?)]
          [write-report-exclude-alignment-pattern (-> natural? hash? hash? path-string? void?)]
          [write-report-exclude-format-information (-> natural? hash? hash? path-string? void?)]
          [write-report-exclude-version-information (-> natural? hash? hash? path-string? void?)]
          [write-report-exclude-dark-module (-> natural? hash? hash? path-string? void?)]
          [write-report-unmask-data (-> list? list? string? path-string? void?)]
          [write-report-ungroup-data (-> list? list? list? list? string? path-string? void?)]
          [write-report-decoded-data (-> string? natural? bytes? list? path-string? void?)]
          [write-report-final-string (-> string? path-string? void?)]
          ))
