(in-package :cl-user)
(defpackage clones-asd
  (:use :cl :asdf))
(in-package :clones-asd)

(defsystem clones
  :version "0.1"
  :author "Brit Butler"
  :license "MIT"
  :homepage "https://clones.kingcons.io"
  :source-control (:git "https://git.sr.ht/~kingcons/clones")
  :description "A work-in-progress Emulator for the Nintendo Entertainment System."
  :depends-on (:alexandria :serapeum :mgl-pax :sdl2 :static-vectors :swank)
  :components ((:module "src"
                :serial t
                :components
                ((:file "util")
                 (:file "rom")
                 (:file "mappers")
                 (:file "ppu")
                 (:file "input")
                 (:file "memory")
                 (:file "opcodes")
                 (:file "disassembler")
                 (:file "cpu")
                 (:file "renderer")
                 (:file "debug")
                 (:file "clones")
                 (:file "docs"))))
  :build-operation "program-op"
  :build-pathname "bin/clones"
  :entry-point "clones:main"
  :in-order-to ((test-op (test-op clones/test))))

(defsystem clones/test
  :version "0.1"
  :author "Brit Butler"
  :license "MIT"
  :description "Tests for Clones."
  :depends-on (:clones :try :zpng :shasht)
  :components ((:module "test"
                :serial t
                :components ((:file "rom")
                             (:file "mappers")
                             (:file "ppu")
                             (:file "memory")
                             (:file "cpu")
                             (:file "renderer")
                             (:file "tests"))))
  :perform (test-op (o s)
             (uiop:symbol-call '#:clones.test '#:test)))
