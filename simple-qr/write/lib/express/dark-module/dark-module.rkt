#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-dark-module (-> hash? natural? path-string? void?)]
          ))

(define (write-report-dark-module points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "dark-module")]
         [scrbl_file (build-path scrbl_dir "dark-module.scrbl")]
         [img_file (build-path scrbl_dir "dark-module.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Dark Module}\n\n")
        (printf "draw dark module.\n")
        (printf "@section{Dark Module Bits}\n")
        (printf (display-qr-bits modules points_map))
        (printf "@section{Dark Module Image}\n")
        (draw modules 5 points_map (make-hash) img_file)
        (printf "@image{dark-module/dark-module.img}")
        ))))
