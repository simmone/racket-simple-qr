#lang racket

(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-mask-list (-> list? list? natural? path-string? void?)]
          ))

(define (write-report-mask-list mask_list penalty_list modules express_path)
  (let* ([scrbl_dir (build-path express_path "mask-list")]
         [scrbl_file (build-path scrbl_dir "mask-list.scrbl")]
         [img0_file (build-path scrbl_dir "mask0.img")]
         [img1_file (build-path scrbl_dir "mask1.img")]
         [img2_file (build-path scrbl_dir "mask2.img")]
         [img3_file (build-path scrbl_dir "mask3.img")]
         [img4_file (build-path scrbl_dir "mask4.img")]
         [img5_file (build-path scrbl_dir "mask5.img")]
         [img6_file (build-path scrbl_dir "mask6.img")]
         [img7_file (build-path scrbl_dir "mask7.img")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Mask Data}\n\n")
        (printf "compare eight types of mask.\n")
        (printf "@section{Mask 0: Penalty: ~a}\n" (list-ref penalty_list 0))
        (draw modules 5 (list-ref mask_list 0) (make-hash) img0_file)
        (printf "@image{mask-list/mask0.img}")
        (printf "@section{Mask 1: Penalty: ~a}\n" (list-ref penalty_list 1))
        (draw modules 5 (list-ref mask_list 1) (make-hash) img1_file)
        (printf "@image{mask-list/mask1.img}")
        (printf "@section{Mask 2: Penalty: ~a}\n" (list-ref penalty_list 2))
        (draw modules 5 (list-ref mask_list 2) (make-hash) img2_file)
        (printf "@image{mask-list/mask2.img}")
        (printf "@section{Mask 3: Penalty: ~a}\n" (list-ref penalty_list 3))
        (draw modules 5 (list-ref mask_list 3) (make-hash) img3_file)
        (printf "@image{mask-list/mask3.img}")
        (printf "@section{Mask 4: Penalty: ~a}\n" (list-ref penalty_list 4))
        (draw modules 5 (list-ref mask_list 4) (make-hash) img4_file)
        (printf "@image{mask-list/mask4.img}")
        (printf "@section{Mask 5: Penalty: ~a}\n" (list-ref penalty_list 5))
        (draw modules 5 (list-ref mask_list 5) (make-hash) img5_file)
        (printf "@image{mask-list/mask5.img}")
        (printf "@section{Mask 6: Penalty: ~a}\n" (list-ref penalty_list 6))
        (draw modules 5 (list-ref mask_list 6) (make-hash) img6_file)
        (printf "@image{mask-list/mask6.img}")
        (printf "@section{Mask 7: Penalty: ~a}\n" (list-ref penalty_list 7))
        (draw modules 5 (list-ref mask_list 7) (make-hash) img7_file)
        (printf "@image{mask-list/mask7.img}")
        ))))
