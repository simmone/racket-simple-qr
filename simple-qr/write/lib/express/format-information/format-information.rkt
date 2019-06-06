#lang racket

(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-format-information (-> string? hash? natural? path-string? void?)]
          ))

(define (write-report-format-information format_str points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "format-information")]
         [scrbl_file (build-path scrbl_dir "format-information.scrbl")]
         [img_file (build-path scrbl_dir "format-information.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Format Information}\n\n")
        (printf "redraw actual format information string.\n")
        (printf "@section{Format String: ~a}\n" format_str)
        (draw modules 5 points_map (make-hash) img_file)
        (printf "@image{format-information/format-information.img}")
        ))))
