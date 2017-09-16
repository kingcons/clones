# Clones

[![Build Status](https://travis-ci.org/kingcons/clones.svg?branch=master)](https://travis-ci.org/kingcons/clones)

## Usage

## Installation

Use [Quicklisp](https://quicklisp.org)!

Just run `(ql:quickload :clones)` and you should be ready to go.

## Tests

Clones is tested with [Prove](https://github.com/fukamachi/prove).
Install clones and then run `(asdf:test-system :clones)`.

If running tests in SLIME, you may want to disable color support
since SLIME doesn't support ANSI color codes.

```lisp
(ql:quickload :clones-test)
(let ((prove:*enable-colors* nil)) (asdf:test-system :clones))
```

## Author

* Brit Butler (brit@kingcons.io)

## Copyright

Copyright (c) 2017 Brit Butler (brit@kingcons.io)

## License

Licensed under the LLGPL License.
