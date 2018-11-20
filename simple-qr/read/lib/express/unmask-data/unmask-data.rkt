 #lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-unmask-data (-> list? list? string? path-string? void?)]
          ))

(define (write-report-unmask-data data_list mask_list unmask_bits express_path)
  (let* ([scrbl_dir (build-path express_path "unmask-data")]
         [scrbl_file (build-path scrbl_dir "unmask-data.scrbl")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Unmask Data}\n\n")
        (printf "get data bits and unmask.\n")
        (printf "@section{Data List}\n")
        (printf (display-list data_list 1 64))
        (printf "@section{Mask List}\n")
        (printf (display-list mask_list 1 64))
        (printf "@section{Unmask Bits}\n")
        (printf (display-list (split-string unmask_bits 8) 10))
        ))))
