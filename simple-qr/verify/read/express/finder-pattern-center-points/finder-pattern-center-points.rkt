#lang racket

(require "../../../../share/func.rkt")
(require "../../../../share/draw/draw.rkt")

(provide (contract-out
          [write-report-finder-pattern-center-points (-> natural? list? natural? list? path-string? void?)]
          ))

(define (write-report-finder-pattern-center-points module_width center_points bits_width points_list express_path)
  (let* ([scrbl_dir (build-path express_path "finder-pattern-center-points")]
         [scrbl_file (build-path scrbl_dir "finder-pattern-center-points.scrbl")]
         [img_file (build-path scrbl_dir "finder-pattern-center-points.img")]
         [points_map (points->points_map points_list)]
         [base1_center_points (points->base1_points center_points)])

    (make-directory* scrbl_dir)
    
    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Guess Finder Pattern and Module Width}\n\n")
        (printf "locate three finder pattern, and probe the module width.\n")
        (printf "@section{Module Width: ~apx}\n" module_width)
        (printf "@section{Center Points}\n")
        (printf "top-left(red): ~a, top-right(yellow): ~a, botterm left(blue): ~a\n"
                (first center_points) (second center_points) (third center_points))
        (printf "@section{Tag Center Points}\n")
        (let ([color_map (make-hash)])
          (hash-set! color_map (first base1_center_points) "red")
          (hash-set! color_map (second base1_center_points) "yellow")
          (hash-set! color_map (third base1_center_points) "blue")
          (draw bits_width 2 points_map color_map img_file))
        (printf "@image{finder-pattern-center-points/finder-pattern-center-points.img}")
        ))))
