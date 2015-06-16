#lang racket

(require racket/string)

(with-output-to-file 
 "code-info-dic.rkt" #:exists 'replace
 (lambda ()
   (let-values ([(bits_list ec_list group_list)
                 (with-input-from-file "error_dic_table.txt"
                   (lambda ()
                     (let loop ([line (string-trim (read-line))]
                                [bits_width_list '()]
                                [block_ec_width_list '()]
                                [block_group_list '()])
                       (if (not (eof-object? line))
                           (let* ([items (regexp-split #rx"\t" line)]
                                  [key (string-append "\"" (list-ref items 0) "\"")]
                                  [bits_width (list-ref items 1)]
                                  [block_ec_width (list-ref items 2)]
                                  [gp1_block_numbers (list-ref items 3)]
                                  [gp1_block_width (list-ref items 4)]
                                  [gp2_block_numbers (if (string=? (list-ref items 5) "") "0" (list-ref items 5))]
                                  [gp2_block_width (if (string=? (list-ref items 6) "") "0" (list-ref items 6))])
                             (loop (read-line) 
                                   (cons (cons key bits_width) bits_width_list)
                                   (cons (cons key block_ec_width) block_ec_width_list)
                                   (cons (cons key (vector (cons gp1_block_numbers gp1_block_width) 
                                                         (cons gp2_block_numbers gp2_block_width)))
                                         block_group_list)))
                           (values (reverse bits_width_list) (reverse block_ec_width_list) (reverse block_group_list))))))])
     (printf "#lang racket\n\n")
     (printf "(provide (contract-out\n")
     (printf "     [*required_bits_table* hash?]\n")
     (printf "     [*required_ec_table* hash?]\n")
     (printf "     [*required_group_table* hash?]\n")
     (printf "     ))\n\n")

     (printf "(define *required_bits_table*\n")
     (printf "  '#hash(\n")
     (for-each
      (lambda (item)
        (printf "         ~a\n" item))
      bits_list)
     (printf "         ))\n\n")

     (printf "(define *required_ec_table*\n")
     (printf "  '#hash(\n")
     (for-each
      (lambda (item)
        (printf "         ~a\n" item))
      ec_list)
     (printf "         ))\n\n")

     (printf "(define *required_group_table*\n")
     (printf "  '#hash(\n")
     (for-each
      (lambda (item)
        (printf "         ~a\n" item))
      group_list)
     (printf "         ))\n\n")

     )))
                 
                 
                 
