#lang racket

(require "write/qr-write-express.rkt")

(require racket/runtime-path)
(define-runtime-path content_directory (build-path "express" "content"))

(make-directory* (build-path "express" "content" "docs"))

(qr-write-express "Life is too short to put up unnecessory stress on everyday, you must work in a place that fuel your personal passion." "chenxiao.svg" #:module_width 20 #:output_type 'svg)

;(qr-write-express "chenxiao" "chenxiao.svg" #:module_width 20 #:output_type 'svg)
