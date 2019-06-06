#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-version-information (-> string? hash? natural? path-string? void?)]
          ))

(define (write-report-version-information version_str points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "version-information")]
         [scrbl_file (build-path scrbl_dir "version-information.scrbl")]
         [img_file (build-path scrbl_dir "version-information.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Version Information}\n\n")
        (printf "draw version information.\n")
        (printf "@section{Version String}\n")
        (printf "@verbatim{[~a]}\n" version_str)
        (printf "@section{Version Information Bits}\n")
        (printf (display-qr-bits modules points_map))
        (printf "@section{Version Information Image}\n")
        (draw modules 5 points_map (make-hash) img_file)
        (printf "@image{version-information/version-information.img}")
        ))))
