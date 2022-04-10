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
  :depends-on (:alexandria :serapeum :mgl-pax)
  :components ((:module "src"
                :serial t
                :components
                ((:file "util")
                 (:file "rom")
                 (:file "mappers")
                 (:file "ppu")
                 (:file "memory")
                 (:file "opcodes")
                 (:file "disassembler")
                 (:file "cpu")
                 (:file "docs")
                 (:file "clones"))))
  :in-order-to ((test-op (test-op clones/test))))

(defsystem clones/test
  :version "0.1"
  :author "Brit Butler"
  :license "MIT"
  :description "Tests for Clones."
  :depends-on (:clones :try)
  :components ((:module "test"
                :serial t
                :components ((:file "rom")
                             (:file "mappers")
                             (:file "ppu")
                             (:file "memory")
                             (:file "cpu")
                             (:file "tests"))))
  :perform (test-op (o s)
             (uiop:symbol-call '#:clones.test '#:test)))
