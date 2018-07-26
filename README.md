# Clones

[![Build Status](https://travis-ci.org/kingcons/clones.svg?branch=master)](https://travis-ci.org/kingcons/clones)

## Goals

See: [Design](https://github.com/kingcons/clones/blob/master/DESIGN.md)

## Playability

![wip](http://redlinernotes.com/images/clones.png)

We're getting close...

## Usage

Sprite rendering isn't fully up and going but if you want to play around a bit,
just try the following in a recent [SBCL][sbcl] or [CCL][ccl]:

```lisp
(ql:quickload :clones)
(in-package :clones)
(change-game "roms/color_test.nes")
(sdl2:make-this-thread-main #'play)
```

### Keybindings

Currently hardcoded in `input.lisp` but you should have an easy time tweaking them.
I'll get support for some kind of config file ... well, as soon as graphics and sound work.

| nes    | pc     |
|--------|--------|
| up     | w      |
| left   | a      |
| down   | s      |
| right  | d      |
| a      | j      |
| b      | k      |
| select | space  |
| start  | return |

## Installation

Make sure you have sdl2-dev installed through apt, homebrew, etc.

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

[sbcl]: http://sbcl.org/
[ccl]: https://ccl.clozure.com/
