#lang racket

(provide (contract-out
          [write-report-basic-information (-> natural? string? string? string? string? path-string? void?)]
          ))

(define (write-report-basic-information version format_string format_information error_level mask_pattern express_path)
  (let* ([scrbl_dir (build-path express_path "basic-information")]
         [scrbl_file (build-path scrbl_dir "basic-information.scrbl")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Basic Information}\n\n")
        (printf "decode basic information.\n")
        (printf "@section{Version: @bold{~a}}\n" version)
        (printf "@section{Format String}\n")
        (printf "@verbatim{~a}" format_string)
        (printf "@section{Format Information}\n")
        (printf "@verbatim{~a}" format_information)
        (printf "@section{Error Level: @bold{~a}}\n" error_level)
        (printf "@section{Mask Pattern: @bold{~a}}\n" mask_pattern)
        ))))
