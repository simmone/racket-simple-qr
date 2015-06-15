#lang racket

(require racket/string)

(with-ouput-to-file 
 "code-info.rkt" #:exists 'replace
 (lambda ()
   (with-input-from-file "error_dic_table.txt"
     (lambda ()
       (let loop ([line (string-trim (read-line))]
                  [bits_width_list '()]
                  [block_ec_width_list '()]
                  [block_group_list '()])
         (if (not (eof-object? line))
               (let* ([items (regexp-split #rx"\t" line)]
                      [key (list-ref items 0)]
                      [bits_width (list-ref items 1)]
                      [block_ec_width (list-ref items 2)]
                      [gp1_block_numbers (list-ref items 3)]
                      [gp1_block_width (list-ref items 4)]
                 )
               (values bits_width_list block_ec_width_list block_group_list)))))
                 
 
                    
