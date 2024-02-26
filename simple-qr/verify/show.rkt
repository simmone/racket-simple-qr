#lang racket

(require "write/qr-write-express.rkt")

(require racket/runtime-path)
(define-runtime-path content_directory (build-path "express" "content"))

(make-directory* (build-path "express" "content" "docs"))

(qr-write-express "chenxiao" "chenxiao.svg" #:output_type 'svg)
