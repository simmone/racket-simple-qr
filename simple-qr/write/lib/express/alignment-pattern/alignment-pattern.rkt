#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-alignment-pattern (-> hash? natural? path-string? void?)]
          ))

(define (write-report-alignment-pattern points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "alignment-pattern")]
         [scrbl_file (build-path scrbl_dir "alignment-pattern.scrbl")]
         [img_file (build-path scrbl_dir "alignment-pattern.img")])

    (with-output-to-file (build-path express_path "report.scrbl") #:exists 'append
      (lambda ()
        (printf "@include-section[\"alignment-pattern/alignment-pattern.scrbl\"]\n\n")))

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Alignment Pattern}\n\n")
        (printf "draw alignment pattern.\n")
        (printf "@section{Alignment Pattern Bits}\n")
        (printf (display-qr-bits modules points_map))
        (printf "@section{Alignment Pattern Image}\n")
        (draw modules 5 points_map (make-hash) '("black" . "white") img_file)
        (printf "@image{alignment-pattern/alignment-pattern.img}")
        ))))
