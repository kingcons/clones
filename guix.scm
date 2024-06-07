;;; Clones --- Common Lisp Observable Nintendo Emulator
;;; Copyright © 2022 Brit Butler <brit@kingcons.io>
;;;
;;; Commentary:
;;
;; GNU Guix development package.  To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell
;;
;; Afterwards, start clones running a swank socket and connect via emacs:
;;
;;  > make repl
;;
;; Happy Hacking!

(use-modules (guix packages)
             (guix licenses)
             (guix git)
             (guix git-download)
             (guix build-system gnu)
             (guix build-system asdf)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages lisp)
             (gnu packages lisp-xyz)
             (gnu packages sdl))

(define-public sbcl-mgl-pax
  (let ((commit "171e93f2209147c9ab8db8ac79eeb2e911156259")
        (revision "0"))
    (package
      (name "sbcl-mgl-pax")
      (version (git-version "0.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/melisgl/mgl-pax")
               (commit commit)))
         (sha256
          (base32 "0zdc5v4z7lysrafvn1nfpjlc8m6142zmx5axqwjwspc9rnl21ml1"))
         (file-name (git-file-name "cl-mgl-pax" version))))
      (build-system asdf-build-system/sbcl)
      ;; (native-inputs
      ;;  (list sbcl-try))
      (inputs
       (list sbcl-3bmd
             sbcl-alexandria
             sbcl-colorize
             sbcl-hunchentoot
             sbcl-md5
             sbcl-named-readtables
             sbcl-pythonic-string-reader
             sbcl-slime-swank
             sbcl-trivial-utf-8))
      (arguments
       `(#:asd-systems '("mgl-pax"
                         "mgl-pax/bootstrap"
                         "mgl-pax/navigate"
                         "mgl-pax/document"
                         "mgl-pax/transcribe"
                         "mgl-pax/web"
                         "mgl-pax/full")
         ;; Tests disabled because of a circular dependency
         ;;   try -> mgl-pax -> try
         #:tests? #f))
      (synopsis "Exploratory programming environment and documentation generator")
      (description
       "PAX provides an extremely poor man's Explorable Programming
environment.  Narrative primarily lives in so called sections that mix markdown
docstrings with references to functions, variables, etc, all of which should
probably have their own docstrings.

The primary focus is on making code easily explorable by using SLIME's
@command{M-.} (@command{slime-edit-definition}).  See how to enable some
fanciness in Emacs Integration.  Generating documentation from sections and all
the referenced items in Markdown or HTML format is also implemented.

With the simplistic tools provided, one may accomplish similar effects as with
Literate Programming, but documentation is generated from code, not vice versa
and there is no support for chunking yet.  Code is first, code must look
pretty, documentation is code.")
      (home-page "https://melisgl.github.io/mgl-pax/")
      (license expat))))

(define-public cl-mgl-pax
  (sbcl-package->cl-source-package sbcl-mgl-pax))

(package
  (name "clones")
  (version "0.0.1")
  (source (git-checkout (url (dirname (current-filename)))))
  (build-system gnu-build-system)
  (native-inputs (list automake sdl2))
  (inputs (list sbcl cl-slime-swank cl-alexandria cl-serapeum cl-mgl-pax cl-static-vectors cl-sdl2))
  (synopsis "Nintendo Emulator in Common Lisp")
  (description "Clones is an NES emulator written in Common Lisp.")
  (home-page "https://clones.kingcons.io/")
  (license expat))
