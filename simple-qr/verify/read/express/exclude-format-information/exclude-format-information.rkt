#lang racket

(require "../../../../share/func.rkt")
(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-exclude-format-information (-> natural? hash? hash? path-string? void?)]
          ))

(define (write-report-exclude-format-information modules points_map exclude_map express_path)
  (let* ([scrbl_dir (build-path express_path "exclude-format-information")]
         [scrbl_file (build-path scrbl_dir "exclude-format-information.scrbl")]
         [img_file (build-path scrbl_dir "exclude-format-information.img")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Exclude Format Information Bits}\n\n")
        (printf "filter format information.\n")
        (printf "@section{Exclude Format Information Image}\n")
        (draw modules 5 points_map exclude_map img_file)
        (printf "@image{exclude-format-information/exclude-format-information.img}")
        ))))
