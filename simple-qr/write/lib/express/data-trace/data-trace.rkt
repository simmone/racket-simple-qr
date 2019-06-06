#lang racket

(require "../../../../share/draw/draw.rkt")
(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-data-trace (-> list? list? hash? natural? path-string? void?)]
          ))

(define (write-report-data-trace trace_list data_list points_map modules express_path)
  (let* ([scrbl_dir (build-path express_path "data-trace")]
         [scrbl_file (build-path scrbl_dir "data-trace.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Data Trace}\n\n")
        (printf "where to draw data bits.\n")
        (printf "@section{Current QR Bits}\n")
        (printf (display-qr-bits modules points_map))
        (printf "@section{Data Cordinate}\n")
        (printf (display-double-list
                 data_list
                 (map (lambda (rec) (format "(~a.~a)" (car rec) (cdr rec))) trace_list)
                 12
                 8))
        ))))
