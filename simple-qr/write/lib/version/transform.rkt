#lang racket

(with-input-from-file "capacity_table.txt"
  (lambda ()
    (let loop ([line (regexp-replace* #rx"\r|\n" (read-line) "")]
               [version #f])
      (when (not (eof-object? line))
        (let ([items (regexp-split #rx"\t" line)])
          (if (and (>= (length items) 5) (regexp-match #rx"^([0-9]+|[A-Z])$" (first items)))
              (if (= (length items) 6)
                  (begin
                    (printf "|~a|~a|~a|~a|~a|~a|\n"
                            (list-ref items 0)
                            (list-ref items 1)
                            (list-ref items 2)
                            (list-ref items 3)
                            (list-ref items 4)
                            (list-ref items 5))
                    (loop (read-line) (list-ref items 0)))
                  (begin
                    (printf "|~a|~a|~a|~a|~a|~a|\n"
                            version
                            (list-ref items 0)
                            (list-ref items 1)
                            (list-ref items 2)
                            (list-ref items 3)
                            (list-ref items 4))
                    (loop (read-line) version)))
          (loop (read-line) version)))))))
