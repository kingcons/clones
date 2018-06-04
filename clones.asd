#|
  This file is a part of clones project.
  Copyright (c) 2017 Brit Butler (brit@kingcons.io)
|#

#|
  Author: Brit Butler (brit@kingcons.io)
|#

(in-package :cl-user)
(defpackage clones-asd
  (:use :cl :asdf))
(in-package :clones-asd)

(defsystem clones
  :version "0.1"
  :author "Brit Butler"
  :license "LLGPL"
  :depends-on (:alexandria :sdl2 :static-vectors)
  :components ((:module "src"
                :components
                ((:file "clones" :depends-on ("display" "disassembler" "instructions"))
                 (:file "display" :depends-on ("ppu"))
                 (:file "disassembler" :depends-on ("addressing" "instruction-data"))
                 (:file "instructions" :depends-on ("ppu" "addressing"))
                 (:file "addressing" :depends-on ("cpu" "memory"))
                 (:file "cpu" :depends-on ("memory" "instruction-data"))
                 (:file "memory" :depends-on ("mappers" "ppu" "input"))
                 (:file "mappers" :depends-on ("rom" "conditions"))
                 (:file "input" :depends-on ("rom"))
                 (:file "ppu" :depends-on ("rom"))
                 (:file "rom" :depends-on ("conditions" "util"))
                 (:file "instruction-data")
                 (:file "conditions")
                 (:file "util"))))
  :in-order-to ((test-op (test-op clones-test))))
