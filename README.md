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


