#lang racket

(require rackunit/text-ui)
(require rackunit "../../../read/qr-read.rkt")
(require rackunit "../../../write/qr-write.rkt")
(require "../../../share/func.rkt")

(require racket/runtime-path)
(define-runtime-path test1_file "test1.png")
(define-runtime-path test2_file "test2.png")
(define-runtime-path test3_file "test3.png")

(define test-qr-write-case1
  (test-suite 
   "test-qr-write"
   
   (test-case
    "test-qr-case1"

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
            (qr-write url test2_file #:express? #t)
            (check-equal? (qr-read test2_file #:express? #t) url)))
        (lambda () (when (file-exists? test2_file)
                         (delete-file test2_file)))))
    ))

(run-tests test-qr-write-case1)
