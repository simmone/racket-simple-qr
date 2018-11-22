#lang racket

(require "header.rkt")
(require "overview/overview.rkt")
(require "input/input.rkt")
(require "start/start.rkt")
(require "finder-pattern/finder-pattern.rkt")
(require "separator/separator.rkt")
(require "timing-pattern/timing-pattern.rkt")
(require "alignment-pattern/alignment-pattern.rkt")
(require "reserved-format-information/reserved-format-information.rkt")
(require "version-information/version-information.rkt")
(require "dark-module/dark-module.rkt")
(require "data-bits/data-bits.rkt")
(require "add-indicator/add-indicator.rkt")
(require "add-terminator/add-terminator.rkt")
(require "add-multiple8/add-multiple8.rkt")
(require "repeat-pad/repeat-pad.rkt")
(require "decimal-list/decimal-list.rkt")
(require "group-decimal-list/group-decimal-list.rkt")
(require "error-code/error-code.rkt")
(require "interleave-data-group/interleave-data-group.rkt")
(require "append-remainder/append-remainder.rkt")
(require "data-trace/data-trace.rkt")
(require "fill-data/fill-data.rkt")
(require "mask-list/mask-list.rkt")
(require "masked/masked.rkt")
(require "format-information/format-information.rkt")
(require "final/final.rkt")

(provide (contract-out
          [write-report-header (-> path-string? void?)]
          [write-report-input (-> string? string? string? natural? natural? natural? path-string? void?)]
          [write-report-overview (-> path-string? void?)]
          [write-report-start (-> natural? path-string? void?)]
          [write-report-finder-pattern (-> hash? natural? path-string? void?)]
          [write-report-separator (-> hash? natural? path-string? void?)]
          [write-report-timing-pattern (-> hash? natural? path-string? void?)]
          [write-report-alignment-pattern (-> hash? natural? path-string? void?)]
          [write-report-reserved-format-information (-> hash? natural? path-string? void?)]
          [write-report-version-information (-> string? hash? natural? path-string? void?)]
          [write-report-dark-module (-> hash? natural? path-string? void?)]
          [write-report-data-bits (-> string? string? path-string? void?)]
          [write-report-add-indicator (-> natural? natural? string? string? string? string? path-string? void?)]
          [write-report-add-terminator (-> natural? natural? string? path-string? void?)]
          [write-report-add-multiple8 (-> string? path-string? void?)]
          [write-report-repeat-pad (-> natural? string? string? path-string? void?)]
          [write-report-decimal-list (-> string? list? path-string? void?)]
          [write-report-group-decimal-list (-> list? list? path-string? void?)]
          [write-report-error-code (-> natural? string? list? path-string? void?)]
          [write-report-interleave-data-group (-> list? string? path-string? void?)]
          [write-report-append-remainder (-> natural? string? path-string? void?)]
          [write-report-data-trace (-> list? list? hash? natural? path-string? void?)]
          [write-report-fill-data (-> hash? natural? path-string? void?)]
          [write-report-mask-list (-> list? list? natural? path-string? void?)]
          [write-report-masked (-> natural? natural? hash? natural? path-string? void?)]
          [write-report-format-information (-> string? hash? natural? path-string? void?)]
          [write-report-final (-> hash? natural? path-string? void?)]
          ))
