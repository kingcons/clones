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
  :depends-on (:alexandria :sdl2)
  :components ((:module "src"
                :components
                ((:file "ppu" :depends-on ("instructions"))
                 (:file "disassembler" :depends-on ("addressing" "instruction-data"))
                 (:file "instructions" :depends-on ("cpu" "addressing"))
                 (:file "addressing" :depends-on ("cpu" "memory"))
                 (:file "cpu" :depends-on ("memory" "instruction-data"))
                 (:file "memory" :depends-on ("mappers" "util"))
                 (:file "mappers" :depends-on ("rom" "conditions"))
                 (:file "rom" :depends-on ("util" "conditions"))
                 (:file "instruction-data")
                 (:file "conditions")
                 (:file "util"))))
  :in-order-to ((test-op (test-op clones-test))))
