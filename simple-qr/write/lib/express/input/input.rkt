#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-input (-> string? string? string? natural? natural? natural? path-string? void?)]
          ))

(require racket/runtime-path)
(define-runtime-path mode_tip "mode_tip")
(define-runtime-path error_level_tip "error_level_tip")

(define (write-report-input data mode error_level version modules module_width express_path)
  (let* ([scrbl_dir (build-path express_path "input")]
         [scrbl_file (build-path scrbl_dir "input.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Input}\n\n")
        (printf "collect input factors.\n")
        (printf "@section{Data}\n")
        (printf "length: ~a\n\n" (string-length data))
        (printf "[@bold{~a}]\n\n" (format-string data 100))
        (printf "100 chars per line.\n\n")
        (printf "@section{Mode: @bold{~a}}\n" mode)
        (printf (file->string mode_tip))
        (printf "@section{Error Level: @bold{~a}}\n" error_level)
        (printf (file->string error_level_tip))
        (printf "@section{Version: @bold{~a}}\n" version)
        (printf "version means capacity, it is decided by mode and error level.\n\n")
        (printf "@section{Modules: @bold{~a}, Module Width: @bold{~a}}\n" modules module_width)
        (printf "modules means qr code width.\n\n @verbatim{modules = (+ 21 (* 4 (sub1 version)))}\n\nmodule width default is 5px.\n\n")
        ))))

