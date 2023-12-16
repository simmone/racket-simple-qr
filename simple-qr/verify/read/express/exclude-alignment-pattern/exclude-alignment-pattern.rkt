#lang racket

(require "../../../../share/func.rkt")
(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-exclude-alignment-pattern (-> natural? hash? hash? path-string? void?)]
          ))

(define (write-report-exclude-alignment-pattern modules points_map exclude_map express_path)
  (let* ([scrbl_dir (build-path express_path "exclude-alignment-pattern")]
         [scrbl_file (build-path scrbl_dir "exclude-alignment-pattern.scrbl")]
         [img_file (build-path scrbl_dir "exclude-alignment-pattern.img")]
         [color_map (make-hash)])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Exclude Alignment Pattern Bits}\n\n")
        (printf "filter alignment pattern.\n")
        (printf "@section{Exclude Alignment Pattern Image}\n")
        (draw modules 5 points_map exclude_map img_file)
        (printf "@image{exclude-alignment-pattern/exclude-alignment-pattern.img}")
        ))))
