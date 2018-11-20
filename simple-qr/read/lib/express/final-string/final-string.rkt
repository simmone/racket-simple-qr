 #lang racket

(require "../../../../share/func.rkt")

(provide (contract-out
          [write-report-final-string (-> string? path-string? void?)]
          ))

(define (write-report-final-string final_string express_path)
  (let* ([scrbl_dir (build-path express_path "final-string")]
         [scrbl_file (build-path scrbl_dir "final-string.scrbl")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Final String}\n\n")
        (printf "final string.\n")
        (printf "@section{Decoded String}\n")
        (printf "@verbatim{[~a]}\n" final_string)
        ))))
