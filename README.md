# racket-simple-qr

A Qr Code Reader and Writer for Racket
==================

# Install
    raco pkg install simple-qr

# Basic Usage
```racket

  (require simple-qr)

  (qr-code "https://github.com/simmone" "normal.png")

  (qr-code "https://github.com/simmone" "small.png" #:module_width 2)

  (qr-code "https://github.com/simmone" "large.png" #:module_width 10)

```
![ScreenShot](simple-qr/example/small.png)

![ScreenShot](simple-qr/example/normal.png)

![ScreenShot](simple-qr/example/large.png)

