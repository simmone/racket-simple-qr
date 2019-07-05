#lang racket

(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-masked (-> natural? natural? hash? natural? path-string? void?)]
          ))

(define (write-report-masked min_penalty mask_index points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "masked")]
         [scrbl_file (build-path scrbl_dir "masked.scrbl")]
         [img_file (build-path scrbl_dir "masked.img")])

    (with-output-to-file (build-path express_path "report.scrbl") #:exists 'append
      (lambda ()
        (printf "@include-section[\"masked/masked.scrbl\"]\n\n")))

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Selected Mask}\n\n")
        (printf "select the mininal penalty mask.\n")
        (printf "@section{Penalty: ~a Mask Index: ~a}\n" min_penalty mask_index)
        (draw modules 5 points_map (make-hash) '("black" . "white") img_file)
        (printf "@image{masked/masked.img}")
        ))))
