#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-reserved-format-information (-> hash? natural? path-string? void?)]
          ))

(define (write-report-reserved-format-information points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "reserved-format-information")]
         [scrbl_file (build-path scrbl_dir "reserved-format-information.scrbl")]
         [img_file (build-path scrbl_dir "reserved-format-information.img")])

    (with-output-to-file (build-path express_path "report.scrbl") #:exists 'append
      (lambda ()
        (printf "@include-section[\"reserved-format-information/reserved-format-information.scrbl\"]\n\n")))

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Reserved Format Information}\n\n")
        (printf "draw reserved format information.\n")
        (printf "@section{Reserved Format Information Bits}\n")
        (printf (display-qr-bits modules points_map))
        (printf "@section{Reserved Format Information Image}\n")
        (draw modules 5 points_map (make-hash) '("black" . "white") img_file)
        (printf "@image{reserved-format-information/reserved-format-information.img}")
        ))))
