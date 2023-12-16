#lang racket

(require "../../../../share/func.rkt")
(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-exclude-timing-pattern (-> natural? hash? hash? path-string? void?)]
          ))

(define (write-report-exclude-timing-pattern modules points_map exclude_map express_path)
  (let* ([scrbl_dir (build-path express_path "exclude-timing-pattern")]
         [scrbl_file (build-path scrbl_dir "exclude-timing-pattern.scrbl")]
         [img_file (build-path scrbl_dir "exclude-timing-pattern.img")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Exclude Timing Pattern Bits}\n\n")
        (printf "filter timing pattern.\n")
        (printf "@section{Exclude Timing Pattern Image}\n")
        (draw modules 5 points_map exclude_map img_file)
        (printf "@image{exclude-timing-pattern/exclude-timing-pattern.img}")
        ))))
