 #lang racket

(provide (contract-out
          [write-report-input (-> path-string? void?)]
          ))

(define (write-report-input express_path)
  (let* ([scrbl_dir (build-path express_path "input")]
         [scrbl_file (build-path scrbl_dir "input.scrbl")]
         [img_file (build-path express_path "input.img")])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{QR Image}\n\n")
        (printf "the qr image will be decoded.\n")
        (printf "@section{Input Image}\n")
        (printf "@image{~a}" img_file)
        ))))
