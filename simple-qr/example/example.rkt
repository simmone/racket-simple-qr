#lang racket

;; (require simple-qr)

(require "../main.rkt")

;; block's default width is 5

(qr-write "https://github.com/simmone" "normal.png")
 
(qr-write "https://github.com/simmone" "small.png" #:module_width 2)

(qr-write "https://github.com/simmone" "large.png" #:module_width 10)

(printf "~a\n~a\n~a\n"
        (qr-read "normal.png")
        (qr-read "small.png")
        (qr-read "large.png"))

(printf "~a\n" (qr-read "damaged.png"))

(qr-write "https://github.com/simmone" "normal.svg" #:output_type 'svg)




