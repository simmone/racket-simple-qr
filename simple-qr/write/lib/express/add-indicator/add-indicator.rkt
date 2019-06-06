#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-add-indicator (-> natural? natural? string? string? string? string? path-string? void?)]
          ))

(define (write-report-add-indicator character_count character_bit_width character_count_indicator mode mode_indicator data_bits express_path)
  (let* ([scrbl_dir (build-path express_path "add-indicator")]
         [scrbl_file (build-path scrbl_dir "add-indicator.scrbl")]
         [img_file (build-path scrbl_dir "add-indicator.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Add Indicator}\n\n")
        (printf "mode indicator + count indicator + data\n")
        (printf "@section{Count Indicator}\n")
        (printf "character_count: @bold{~a}, character_indicator_width: {~a}, character_count_indicator: {~a}\n"
                character_count character_bit_width character_count_indicator)
        (printf "@section{Mode Indicator}\n")
        (printf "mode: {~a}, mode_indicator: {~a}" mode mode_indicator)
        (printf "@section{Bits added indicators}\n")
        (printf (display-list 
                 (split-string data_bits 8)
                 12))
        ))))
