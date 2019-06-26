# racket-simple-qr

A Qr Code Writer and Reader for Racket
==================

# Install
    raco pkg install simple-qr

# Usage

```racket
(qr-write
        [data (string?)]
        [output_file_path (path-string?)]
        [#:mode mode string? "B"]
        [#:error_level error_level string? "H"]
        [#:module_width module_width natural? 5]
        [#:color color (cons/c string? string?) '("black" . "white")]
        [#:express? express? boolean? #f]
        [#:express_path express_path path-string? "imgfile + '.write.express'"]
        [#:output_type output_type (or/c 'png 'svg)]
        )
      void?
```
  output qr code image to file.
  
  color's form is '(front_color . background_color).
  
  use color "transparent" to set transparent(background).

  if output_type is png, can use @racket[color-database<%>], 
  but hex color is all available in all formats, recommended.

```racket
(qr-read
          [image_file_path (path-string?)]
          [#:express? express? boolean? #f]
          [#:express_path express_path path-string? "imgfile + '.read.express'"]
          )
        string?
```
  read qr code image's content, if failed, return "".

# Example

```racket
#lang racket

(require simple-qr)

;; block's default width is 5

(qr-write "https://github.com/simmone" "normal.png")

(qr-write "https://github.com/simmone" "normal_color.png" #:color '("#ffbb33" . "#0d47a1"))

(qr-write "https://github.com/simmone" "normal_trans.png" #:color '("#9933CC" . "transparent"))
 
(qr-write "https://github.com/simmone" "small.png" #:module_width 2)

(qr-write "https://github.com/simmone" "large.png" #:module_width 10)

(printf "~a\n~a\n~a\n"
        (qr-read "normal.png")
        (qr-read "small.png")
        (qr-read "large.png"))

(printf "~a\n" (qr-read "damaged.png"))

(qr-write "https://github.com/simmone" "normal.svg" #:output_type 'svg)

(qr-write "https://github.com/simmone" "normal_color.svg" #:color '("#ffbb33" . "#0d47a1") #:output_type 'svg)

(qr-write "https://github.com/simmone" "normal_trans.svg" #:color '("#9933CC" . "transparent") #:output_type 'svg)

```

# Png

```racket
(qr-write "https://github.com/simmone" "normal.png")
```
![ScreenShot](simple-qr/example/normal.png)

```racket
(qr-write "https://github.com/simmone" "normal_color.png" #:color '("#ffbb33" . "#0d47a1"))
```
![ScreenShot](simple-qr/example/normal_color.png))

```racket
(qr-write "https://github.com/simmone" "normal_trans.png" #:color '("#9933CC" . "transparent"))
```
![ScreenShot](simple-qr/example/normal_trans.png)

```racket
(qr-write "https://github.com/simmone" "small.png" #:module_width 2)
```
![ScreenShot](simple-qr/example/small.png)

```racket
(qr-write "https://github.com/simmone" "large.png" #:module_width 10)
```
![ScreenShot](simple-qr/example/large.png)

# SVG

```racket
(qr-write "https://github.com/simmone" "normal.svg"  #:output_type 'svg)
```
![ScreenShot](simple-qr/example/normal.svg)

```racket
(qr-write "https://github.com/simmone" "normal_color.svg" #:color '("#ffbb33" . "#0d47a1") #:output_type 'svg)
```
![ScreenShot](simple-qr/example/normal_color.svg)

```racket
(qr-write "https://github.com/simmone" "normal_trans.svg" #:color '("#9933CC" . "transparent") #:output_type 'svg)
```
![ScreenShot](simple-qr/example/normal_trans.svg)

# Read and Correct

```racket
(printf "~a\n" (qr-read "damaged.png"))
```
![ScreenShot](simple-qr/example/damaged.png)

https://github.com/simmone

# Express
    If you want to see the each step of read or write a qr code, can set #:express? to true.
    
    Default will create folder .read.express for qr-read or .write.express for qr-write.

    You can use #:express_path to specify another folder name.

    Warning: express will generate a set of scribble files, it's very slow, debug usage only.

    Then into the express folder, "scribble --htmls report.scrbl" to generate a detail report.

