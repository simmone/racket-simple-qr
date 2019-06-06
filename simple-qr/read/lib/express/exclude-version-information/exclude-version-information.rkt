#lang racket

(require "../../../../share/func.rkt")
(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-exclude-version-information (-> natural? hash? hash? path-string? void?)]
          ))

(define (write-report-exclude-version-information modules points_map exclude_map express_path)
  (let* ([scrbl_dir (build-path express_path "exclude-version-information")]
         [scrbl_file (build-path scrbl_dir "exclude-version-information.scrbl")]
         [img_file (build-path scrbl_dir "exclude-version-information.img")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Exclude Version Information Bits}\n\n")
        (printf "filter version information.\n")
        (printf "@section{Exclude Version Information Image}\n")
        (draw modules 5 points_map exclude_map img_file)
        (printf "@image{exclude-version-information/exclude-version-information.img}")
        ))))
