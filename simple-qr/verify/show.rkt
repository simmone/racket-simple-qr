#lang racket

(require "write/qr-write-express.rkt")

(require racket/runtime-path)
(define-runtime-path content_directory (build-path "express" "content"))

;(delete-directory/files content_directory)

;(make-directory content_directory)

(qr-write-express "chenxiao" "chenxiao.png")
