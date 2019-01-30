# racket-simple-qr

A Qr Code Writer and Reader for Racket
==================

# Install
    raco pkg install simple-qr

# Basic Usage
```racket

  (require simple-qr)

  (qr-write "https://github.com/simmone" "normal.png")

  (qr-write "https://github.com/simmone" "small.png" #:module_width 2)

  (qr-write "https://github.com/simmone" "large.png" #:module_width 10)

  (printf "~a\n~a\n~a\n"
          (qr-read "normal.png")
          (qr-read "small.png")
          (qr-read "large.png"))
```
![ScreenShot](simple-qr/example/small.png)

![ScreenShot](simple-qr/example/normal.png)

![ScreenShot](simple-qr/example/large.png)

https://github.com/simmone

https://github.com/simmone

https://github.com/simmone

![ScreenShot](simple-qr/example/damaged.png)

  (printf "~a\n" (qr-read "damaged.png"))

https://github.com/simmone

# Express
    If you want to see the each step of read or write a qr code, can set #:express? to true.
    
    Default will create folder .read.express for qr-read or .write.express for qr-write.

    You can use #:express_path to specify another folder name.

    Warning: express will generate a set of scribble files, it's very slow, debug usage only.

    Then into the express folder, "scribble --htmls report.scrbl" to generate a detail report.

