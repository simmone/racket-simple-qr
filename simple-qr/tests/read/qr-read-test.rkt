#lang racket

(require rackunit/text-ui)
(require rackunit "../../read/qr-read.rkt")
(require "../../share/func.rkt")

(require racket/runtime-path)
(define-runtime-path normal_file "normal.png")
(define-runtime-path real_file "real.jpg")
(define-runtime-path wiki1_file "wiki1.png")
(define-runtime-path wiki2_file "wiki2.png")

(define test-qr-read
  (test-suite 
   "test-qr-read"
   
   (test-case
    "test-qr-read"

    (check-equal? (qr-read real_file) "http://www.bjhzbw.com")

    (check-equal? (qr-read normal_file) "https://github.com/simmone")

    (check-equal? (qr-read wiki1_file) "http://en.m.wikipedia.org")

    (check-equal? (qr-read wiki2_file) "")
    )

   ))

(run-tests test-qr-read)
