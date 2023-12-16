#lang racket

(require "../../../../share/func.rkt")
(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-exclude-finder-pattern (-> natural? hash? hash? path-string? void?)]
          ))

(define (write-report-exclude-finder-pattern modules points_map exclude_map express_path)
  (let* ([scrbl_dir (build-path express_path "exclude-finder-pattern")]
         [scrbl_file (build-path scrbl_dir "exclude-finder-pattern.scrbl")]
         [img_file (build-path scrbl_dir "exclude-finder-pattern.img")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Exclude Finder Pattern Bits}\n\n")
        (printf "filter finder pattern.\n")
        (printf "@section{Exclude Finder Pattern Image}\n")
        (draw modules 5 points_map exclude_map img_file)
        (printf "@image{exclude-finder-pattern/exclude-finder-pattern.img}")
        ))))
