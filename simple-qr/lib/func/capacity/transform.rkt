#lang racket

(with-output-to-file "capacity.rkt"
  #:exists 'replace
  (lambda ()

    (printf "~a\n\n" "#lang racket")
    (printf "~a\n" "(provide (contract-out")
    (printf "~a\n" "  [*capacity_table* hash?]")
    (printf "~a\n\n" "  ))")
    (printf "~a\n" "(define *capacity_table*")
    (printf "~a\n" "  '#hash(")

    (with-input-from-file "capacity_table.txt"
      (lambda ()
        (let ([version ""])
          (let loop ([line (regexp-replace* #rx"\r|\n" (read-line) "")])
            (when (not (eof-object? line))
                  (let ([items (regexp-split #rx"\t" line)])
                    (when (and (>= (length items) 5) (regexp-match #rx"^([0-9]+|[A-Z])$" (first items)))
                          (let ([error_level ""]
                                [mode_n_num ""]
                                [mode_a_num ""]
                                [mode_m_num ""]
                                [mode_k_num ""])
                            (if (regexp-match #rx"[0-9]+" (first items))
                                (begin
                                  (set! version (first items))
                                  (set! error_level (list-ref items 1))
                                  (set! mode_n_num (list-ref items 2))
                                  (set! mode_a_num (list-ref items 3))
                                  (set! mode_m_num (list-ref items 4))
                                  (set! mode_k_num (list-ref items 5)))
                                (begin
                                  (set! error_level (list-ref items 0))
                                  (set! mode_n_num (list-ref items 1))
                                  (set! mode_a_num (list-ref items 2))
                                  (set! mode_m_num (list-ref items 3))
                                  (set! mode_k_num (list-ref items 4))))
                            (printf "         ~a ~a ~a ~a\n"
                                    (string-append "(\"N-" error_level "-" version "\" . " mode_n_num ")")
                                    (string-append "(\"A-" error_level "-" version "\" . " mode_a_num ")")
                                    (string-append "(\"B-" error_level "-" version "\" . " mode_m_num ")")
                                    (string-append "(\"K-" error_level "-" version "\" . " mode_k_num ")")))))
                  (loop (read-line)))))))

    (printf "~a\n" "         ))")))

