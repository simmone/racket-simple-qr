#lang scribble/manual

@title{QR Code Read Report}

report the process of reading a qr code.

@table-of-contents[]

@include-section["input/input.scrbl"]

@include-section["origin-bits/origin-bits.scrbl"]

@include-section["bw-bits/bw-bits.scrbl"]

@include-section["finder-pattern-center-points/finder-pattern-center-points.scrbl"]

@include-section["rotate-ratio/rotate-ratio.scrbl"]

@include-section["rotated-bits/rotated-bits.scrbl"]

@include-section["trimed-bits/trimed-bits.scrbl"]

@include-section["final-bits/final-bits.scrbl"]

@include-section["basic-information/basic-information.scrbl"]

@include-section["exclude-finder-pattern/exclude-finder-pattern.scrbl"]

@include-section["exclude-separator/exclude-separator.scrbl"]

@include-section["exclude-timing-pattern/exclude-timing-pattern.scrbl"]

@include-section["exclude-alignment-pattern/exclude-alignment-pattern.scrbl"]

@include-section["exclude-format-information/exclude-format-information.scrbl"]

@include-section["exclude-version-information/exclude-version-information.scrbl"]

@include-section["exclude-dark-module/exclude-dark-module.scrbl"]

@include-section["unmask-data/unmask-data.scrbl"]

@include-section["ungroup-data/ungroup-data.scrbl"]

@include-section["decoded-data/decoded-data.scrbl"]

@include-section["final-string/final-string.scrbl"]
