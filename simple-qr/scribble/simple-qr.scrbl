#lang scribble/manual

@(require (for-label racket))

@title{Simple-Qr: Qr Code generator}

@author+email["Chen Xiao" "chenxiao770117@gmail.com"]

simple-qr package is a simple tool to generate your own qr code.

@table-of-contents[]

@section[#:tag "install"]{Install}

raco pkg install simple-qr

@section{Usage}  

@defmodule[simple-qr]
@(require (for-label simple-qr))

@defproc[(qr-code
              [data (string?)]
              [output_file_path (path-string?)]
              [#:mode mode string?]
              [#:error_level error_level string?]
              [#:module_width module_width exact_nonngegative-integer?])
            void?]{
  output qr code image to file.
}

@verbatim{
  #lang racket

  (require simple-qr)
  
  (qr-code "Hello World!" "hello.png")
}

@image{scribble/hello.png}

You can use the optional parameter #:module_width to control the size of image.

module_width means a block(black or white)'s width. 5 is the default.

@verbatim{
  #lang racket

  (require simple-qr)

  (qr-code "https://github.com/simmone" "normal.png")

  (qr-code "https://github.com/simmone" "small.png" #:module_width 2)

  (qr-code "https://github.com/simmone" "large.png" #:module_width 10)
}

@image{example/small.png}

@image{example/normal.png}

@image{example/large.png}
            
@section{FAQ}

@subsection{What mode and error_level I can use?}

qr-code select Byte Mode as it's default mode, The highest error-correct level "H".

All the iso8859-1(latin-1) code include in this mode.

If you want to use Alphanumeric Mode or Numeric Mode, you can use #:mode "A" or #:mode "N" keyword.

If you want to use other error-correct level, use #:error_level to specify a new one("L" "M" "Q" "H").
