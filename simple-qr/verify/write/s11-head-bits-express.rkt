#lang racket

(require racket/runtime-path
         "../../share/qr.rkt"
         "../../share/lib.rkt")

(define-runtime-path s11_head_bits_directory "../express/content/docs/s11_head_bits")
(define-runtime-path index_md_file "../express/content/docs/s11_head_bits/_index.md")

(provide (contract-out
          [s11-head-bits-express (-> string? QR? void?)]))

(define (s11-head-bits-express bits_str qr)
   (s11-head-bits-express s2_character_count s2_1_count_bit_width s3_character_count_indicator s4_mode_indicator s5_header_added_bits qr)
s1_data_bits qr)
s1_data_bits qr)
  (make-directory* s11_head_bits_directory)

  (with-output-to-file index_md_file
    #:exists 'replace
    (lambda ()
      (printf "---\n")
      (printf "weight: 10\n")
      (printf "title: \"Step10: Data to bits\"\n")
      (printf "---\n\n")
      
      (printf "# Data to bits\n\n")
      
      (printf "## head_bits\n\n")
      (printf "convert [~a] to bits on mode[~a]\n\n" (QR-data qr) (QR-mode qr))
      (printf "data string to bits:\n~a\n" (string-to-bits-markdown-table (QR-data qr) bits_str))
      )))

