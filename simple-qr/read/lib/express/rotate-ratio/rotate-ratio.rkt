#lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-rotate-ratio (-> number? path-string? void?)]
          ))

(define (write-report-rotate-ratio rotate_ratio express_path)
  (let* ([scrbl_dir (build-path express_path "rotate-ratio")]
         [scrbl_file (build-path scrbl_dir "rotate-ratio.scrbl")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Rotate Ratio}\n\n")
        (printf "from finder pattern center points, calcuate the rotate ratio needed.\n")
        (printf "@section{Ratio:~a}\n" rotate_ratio)
        ))))
