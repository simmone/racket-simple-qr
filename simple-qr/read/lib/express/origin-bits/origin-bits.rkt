 #lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-origin-bits (-> natural? natural? list? path-string? void?)]
          ))

(define (write-report-origin-bits origin_height origin_width points_list express_path)
  (let* ([scrbl_dir (build-path express_path "origin-bits")]
         [scrbl_file (build-path scrbl_dir "origin-bits.scrbl")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Image To Bits}\n\n")
        (printf "turn bitmap to bits list.\n")
        (printf "@section{height:~a width:~a}\n" origin_height origin_width)
        (printf "bits matrix.\n")
        (printf "@section{Origin Bits}\n")
        (printf (display-list (flatten points_list) 5 origin_width))
        ))))
