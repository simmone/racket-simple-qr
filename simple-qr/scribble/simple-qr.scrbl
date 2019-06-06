#lang scribble/manual

@(require (for-label racket))

@title{Simple-Qr: QR-Code Writer and Reader}

@author+email["Chen Xiao" "chenxiao770117@gmail.com"]

simple-qr package is a simple tool to write or read QR-Code.

@table-of-contents[]

@section[#:tag "install"]{Install}

raco pkg install simple-qr

@section{Usage}

@defmodule[simple-qr]
@(require (for-label simple-qr))

@subsection{Function}

@defproc[(qr-write
              [data (string?)]
              [output_file_path (path-string?)]
              [#:mode mode string? "B"]
              [#:error_level error_level string? "H"]
              [#:module_width module_width natural? 5]
              [#:express? express? boolean? #f]
              [#:express_path express_path path-string? "imgfile + '.write.express'"]
              [#:output_type output_type (or/c 'png 'svg)]
              )
            void?]{
  output qr code image to file.
}

@defproc[(qr-read
                [image_file_path (path-string?)]
                [#:express? express? boolean? #f]
                [#:express_path express_path path-string? "imgfile + '.read.express'"]
                )
              string?]{
  read qr code image's content, if failed, return "".
}

@subsection{Example}

@verbatim{
  #lang racket

  (require simple-qr)

  ;; block's default width is 5
  (qr-write "https://github.com/simmone" "normal.png")

  (qr-write "https://github.com/simmone" "small.png" #:module_width 2)

  (qr-write "https://github.com/simmone" "large.png" #:module_width 10)

  (printf "~a\n~a\n~a\n"
          (qr-read "normal.png")
          (qr-read "small.png")
          (qr-read "large.png"))

  (printf "~a\n" (qr-read "damaged.png"))

  (qr-write "https://github.com/simmone" "normal.svg" #:output_type 'svg)
}

@subsection{Png}

@image{example/small.png}

@image{example/normal.png}

@image{example/large.png}

@subsection{SVG}

@image{example/normal.svg}

@subsection{Read and Correct}

@image{example/damaged.png}

https://github.com/simmone

@section{Express}

If you want to see the each step of read or write a qr code, can set #:express? to true.

Default will create folder .read.express for qr-read or .write.express for qr-write.

You can use #:express_path to specify another folder name.

Warning: express will generate a set of scribble files, it's very slow, debug usage only.

Then into the express folder, @verbatim{scribble --htmls report.scrbl} to generate a detail report.