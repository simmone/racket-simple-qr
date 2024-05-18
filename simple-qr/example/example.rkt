#lang racket

(require "../main.rkt")

(printf "write qr code:\n")

(qr-write "https://github.com/simmone" "normal.png")

(qr-write "https://github.com/simmone" "normal_color.png" #:color '("#ffbb33" . "#0d47a1"))

(qr-write "https://github.com/simmone" "normal_trans.png" #:color '("#9933CC" . transparent))

(qr-write "https://github.com/simmone" "small.png" #:module_width 2)

(qr-write "https://github.com/simmone" "large.png" #:module_width 10)

(qr-write "https://github.com/simmone" "normal.svg" #:output_type 'svg)

(qr-write "https://github.com/simmone" "normal_color.svg" #:color '("#ffbb33" . "#0d47a1") #:output_type 'svg)

(qr-write "https://github.com/simmone" "normal_trans.svg" #:color '("#9933CC" . transparent) #:output_type 'svg)
