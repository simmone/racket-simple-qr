#lang racket

(let ([data_map (make-hash)])
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
                              [mode_k_num ""]
                              [seq 1])
                          (if (regexp-match #rx"[0-9]+" (first items))
                              (set! version (first items))
                              (set! seq 0))

                          (set! error_level (list-ref items seq))
                          (set! seq (add1 seq))

                          (let loop_mode ([loop_list '("N" "A" "B" "K")]
                                          [loop_seq seq])
                            (when (not (null? loop_list))
                                  (let ([key (string-append (car loop_list) "-" error_level)])
                                    (if (hash-has-key? data_map key)
                                        (hash-set! data_map key `(,@(hash-ref data_map key) ,(cons version (list-ref items loop_seq))))
                                        (hash-set! data_map key `(,(cons version (list-ref items loop_seq))))))
                                  (loop_mode (cdr loop_list) (add1 loop_seq)))))))
                (loop (read-line)))))))

  (with-output-to-file "capacity-dic.rkt"
    #:exists 'replace
    (lambda ()

      (printf "~a\n\n" "#lang racket")
      (printf "~a\n" "(provide (contract-out")
      (printf "~a\n" "  [*capacity_table* hash?]")
      (printf "~a\n\n" "  ))")
      (printf "~a\n" "(define *capacity_table*")
      (printf "~a\n" "  '#hash(")

      (hash-for-each
       data_map
       (lambda (key version_pair_list)
         (printf "         (\"~a\" . ~a)\n" key (sort version_pair_list #:key car (lambda (a b) (< (string->number a) (string->number b)))))))

      (printf "~a\n" "         ))"))))

