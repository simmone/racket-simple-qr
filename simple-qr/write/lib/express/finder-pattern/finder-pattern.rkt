#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-finder-pattern (-> hash? natural? path-string? void?)]
          ))

(define (write-report-finder-pattern points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "finder-pattern")]
         [scrbl_file (build-path scrbl_dir "finder-pattern.scrbl")]
         [img_file (build-path scrbl_dir "finder-pattern.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Finder-Pattern}\n\n")
        (printf "draw finder pattern.\n")
        (printf "@section{Finder Pattern Bits}\n")
        (printf (display-qr-bits modules points_map))
        (printf "@section{Finder Pattern Image}\n")
        (draw modules 5 points_map (make-hash) img_file)
        (printf "@image{finder-pattern/finder-pattern.img}")
        ))))
