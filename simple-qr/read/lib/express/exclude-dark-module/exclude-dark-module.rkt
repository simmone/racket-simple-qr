#lang racket

(require "../../../../share/func.rkt")
(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-exclude-dark-module (-> natural? hash? hash? path-string? void?)]
          ))

(define (write-report-exclude-dark-module modules points_map exclude_map express_path)
  (let* ([scrbl_dir (build-path express_path "exclude-dark-module")]
         [scrbl_file (build-path scrbl_dir "exclude-dark-module.scrbl")]
         [img_file (build-path scrbl_dir "exclude-dark-module.img")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Exclude Dark Module Bits}\n\n")
        (printf "filter dark module.\n")
        (printf "@section{Exclude Dark Module Image}\n")
        (draw modules 5 points_map exclude_map img_file)
        (printf "@image{exclude-dark-module/exclude-dark-module.img}")
        ))))
