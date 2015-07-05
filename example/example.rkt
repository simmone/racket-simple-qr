#lang racket

;(require simple-qr)

(require "../simple-qr/main.rkt")

;; block's default width is 5
(qr-code "https://github.com/simmone" "normal.png")

(qr-code "https://github.com/simmone" "small.png" #:module_width 2)

(qr-code "https://github.com/simmone" "large.png" #:module_width 10)



