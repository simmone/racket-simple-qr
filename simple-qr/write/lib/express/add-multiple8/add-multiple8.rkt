#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-add-multiple8 (-> string? path-string? void?)]
          ))

(define (write-report-add-multiple8 multiple8_added_bits express_path)
  (let* ([scrbl_dir (build-path express_path "add-multiple8")]
         [scrbl_file (build-path scrbl_dir "add-multiple8.scrbl")]
         [img_file (build-path scrbl_dir "add-multiple8.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Add Multiple8}\n\n")
        (printf "append '0' to bits to equal multiple 8")
        (printf "@section{Bits added multiple8}\n")
        (printf (display-list 
                 (split-string multiple8_added_bits 8)
                 12))
        ))))
