#lang racket

(require rackunit/text-ui)
(require rackunit "../../read/qr-read.rkt")
(require rackunit "../../write/qr-write.rkt")
(require "../../share/func.rkt")

(require racket/runtime-path)
(define-runtime-path test1_file "test1.png")
(define-runtime-path test2_file "test2.png")
(define-runtime-path test3_file "test3.png")
(define-runtime-path test4_file "test4.png")

(define test-qr-write-cases
  (test-suite 
   "test-qr-write"
   
   (test-case
    "test-qr-cases"

    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([url "https://goooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooogle.com"])
            (qr-write url test1_file)
            (check-equal? (qr-read test1_file) url)))
        (lambda () (when (file-exists? test1_file)
                         (delete-file test1_file)))))

   (test-case
    "test-qr-case2"

    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([url "https://goooooooooskdjflksjdflkasjdlkfjsadflkajsdlfjaslkdfjklsoooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooogle.com"])
            (qr-write url test2_file)
            (check-equal? (qr-read test2_file) url)))
        (lambda () (when (file-exists? test2_file)
                         (delete-file test2_file)))))

    (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([data "01_900022_X257EE9MBEASFA4L"])
            (qr-write data test1_file)
            (check-equal? (qr-read test1_file) data)))
        (lambda () (when (file-exists? test1_file)
                         (delete-file test1_file))))
    ))

(run-tests test-qr-write-cases)
