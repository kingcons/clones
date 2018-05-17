# Clones

[![Build Status](https://travis-ci.org/kingcons/clones.svg?branch=master)](https://travis-ci.org/kingcons/clones)

## Goals

See: [Design](https://github.com/kingcons/clones/blob/master/DESIGN.md)

## Usage

Not just yet...

## Installation

Eventually, Clones will be installable through [Quicklisp](https://quicklisp.org).

For now, you may clone this repo into `~/quicklisp/local-projects`.
Then run `(ql:quickload :clones)` and you should be ready to go!

## Tests

Clones is tested with [Prove](https://github.com/fukamachi/prove).
Install clones and then run `(asdf:test-system :clones)`.

If running tests in SLIME, you may want to disable color support
since SLIME doesn't support ANSI color codes.

```lisp
(ql:quickload :clones-test)
(let ((prove:*enable-colors* nil)) (asdf:test-system :clones))
```

There is also a helper for running individual test files.

```lisp
(ql:quickload :clones-test)
(clones-test.helpers:run-file "cpu")
```

## Author

* Brit Butler (brit@kingcons.io)

## Copyright

Copyright (c) 2018 Brit Butler (brit@kingcons.io)

## License

Licensed under the LLGPL License.
