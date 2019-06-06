#lang racket

(require "../../../../share/func.rkt")
(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-exclude-separator (-> natural? hash? hash? path-string? void?)]
          ))

(define (write-report-exclude-separator modules points_map exclude_map express_path)
  (let* ([scrbl_dir (build-path express_path "exclude-separator")]
         [scrbl_file (build-path scrbl_dir "exclude-separator.scrbl")]
         [img_file (build-path scrbl_dir "exclude-separator.img")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Exclude Separator Bits}\n\n")
        (printf "filter separator.\n")
        (printf "@section{Exclude Separator Image}\n")
        (draw modules 5 points_map exclude_map img_file)
        (printf "@image{exclude-separator/exclude-separator.img}")
        ))))
