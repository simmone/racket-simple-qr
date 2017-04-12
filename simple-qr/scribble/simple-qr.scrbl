#lang scribble/manual

@(require (for-label racket))

@title{Simple-Qr: QR-Code generator}

@author+email["Chen Xiao" "chenxiao770117@gmail.com"]

simple-qr package is a simple tool to access QR-Code.

@table-of-contents[]

@section[#:tag "install"]{Install}

raco pkg install simple-qr

@section{Usage}  

@defmodule[simple-qr]
@(require (for-label simple-qr))

@defproc[(qr-write
              [data (string?)]
              [output_file_path (path-string?)]
              [#:mode mode string?]
              [#:error_level error_level string?]
              [#:module_width module_width exact_nonngegative-integer?])
            void?]{
  output qr code image to file.
}

@defproc[(qr-read
                [image_file_path (path-string?)])
              string?]{ 
  read qr code image's content, if failed, return "".
}

@verbatim{
  #lang racket

  (require simple-qr)

  ;; block's default width is 5
  (qr-code "https://github.com/simmone" "normal.png")

  (qr-code "https://github.com/simmone" "small.png" #:module_width 2)

  (qr-code "https://github.com/simmone" "large.png" #:module_width 10)

  (printf "~a\n~a\n~a\n"
          (qr-read "normal.png")
          (qr-read "small.png")
          (qr-read "large.png"))
}

@image{example/small.png}

@image{example/normal.png}

@image{example/large.png}

https://github.com/simmone

https://github.com/simmone

https://github.com/simmone
            
@section{FAQ}

@subsection{What mode and error_level I can use?}

qr-code select Byte Mode as it's default mode, The highest error-correct level "H".

All the iso8859-1(latin-1) code include in this mode.

If you want to use Alphanumeric Mode or Numeric Mode, you can use #:mode "A" or #:mode "N" keyword.

If you want to use other error-correct level, use #:error_level to specify a new one("L" "M" "Q" "H").

@subsection{qr-read can deal on all kinds of the code images?}

No, sorry, I tried, till now, qr-read only read some very "good" QR-Code, and must use Byte Mode and no error correction.
