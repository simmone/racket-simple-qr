#lang racket

(provide (contract-out
          [qr-code (->* (string?) (#:mode string? #:error_level string? #:module_width exact-nonnegative-integer?) void?)]
          ))

(require "lib/finder-pattern/finder-pattern.rkt")
(require "lib/separator/separator.rkt")
(require "lib/format-information/format-information.rkt")
(require "lib/version-information/version-information.rkt")
(require "lib/timing-pattern/timing-pattern.rkt")
(require "lib/alignment-pattern/alignment-pattern.rkt")
(require "lib/dark-module/dark-module.rkt")
(require "lib/data-encoding/data-encoding.rkt")
(require "lib/fill-data/fill-data.rkt")
(require "lib/mask-data/mask-data.rkt")
(require "lib/lib.rkt")
(require "lib/func/func.rkt")

(require racket/draw)

(define (qr-code data #:mode [mode "B"] #:error_level [error_level "H"] #:module_width [module_width 10])
  (let* ([version (get-version data mode error_level)]
         [modules (version->modules version)])

    (trace (format "start version:~a mode:~a error_level:~a data_length:~a modules:~a\n" version mode error_level (string-length data) modules) 1)

    (let* ([points_map (make-hash)]
           [type_map (make-hash)]
           [sum_count (* modules modules)])

      (draw-finder-pattern modules points_map type_map)
      (draw-separator modules points_map type_map)
      (draw-timing-pattern modules points_map type_map)
      (draw-alignment-pattern version points_map type_map)
      (draw-reserved-format-information modules points_map type_map)
      (draw-reserved-version-information version modules points_map type_map)
      (draw-dark-module version points_map type_map)
      
      (trace (format "finder[~a]separator[~a]timing[~a]alignment[~a]format[~a]version[~a]\n"
                     (length (filter (lambda (rec) (string=? (cdr rec) "finder")) (hash->list type_map)))
                     (length (filter (lambda (rec) (string=? (cdr rec) "separator")) (hash->list type_map)))
                     (length (filter (lambda (rec) (string=? (cdr rec) "timing")) (hash->list type_map)))
                     (length (filter (lambda (rec) (string=? (cdr rec) "alignment")) (hash->list type_map)))
                     (length (filter (lambda (rec) (string=? (cdr rec) "format")) (hash->list type_map)))
                     (length (filter (lambda (rec) (string=? (cdr rec) "version")) (hash->list type_map))))
             1)
      
      (trace (format "remain_points:~a\n" (- sum_count (hash-count points_map))) 1)

      (let ([data_list (string->list (matrix-data data #:version version #:mode mode #:error_level error_level))]
            [trace_list (snake-modules modules #:skip_points_hash points_map)])
        (trace (format "data_length:~a trace_length:~a\n" (length data_list) (length trace_list)) 1)
        (draw-data data_list trace_list points_map type_map))

      (let ([mask_number (mask-data modules points_map type_map)])
        (draw-format-information error_level mask_number modules points_map type_map))

      (draw-version-information version modules points_map type_map)

      (let* ([canvas_width (* (+ modules 8) module_width)]
             [target (make-bitmap canvas_width canvas_width)]
             [dc (new bitmap-dc% [bitmap target])])

        (draw-background dc (+ modules 8) module_width)

        (draw-points dc module_width points_map)

        (send target save-file "box.png" 'png)
        
        (system "open box.png")))))

(parameterize ([*trace_level* 2])
              (qr-code "SM" #:mode "A" #:error_level "M"))
