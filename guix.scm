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
