# racket-simple-qr

A Qr Code Generator for Racket
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
![ScreenShot](example/small.png)

![ScreenShot](example/normal.png)

![ScreenShot](example/large.png)

