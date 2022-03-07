(in-package :cl-user)
(defpackage clones-asd
  (:use :cl :asdf))
(in-package :clones-asd)

(defsystem clones
  :version "0.1"
  :author "Brit Butler"
  :license "MIT"
  :homepage "http://github.com/kingcons/clones"
  :bug-tracker "http://github.com/kingcons/clones/issues"
  :source-control (:git "https://github.com/kingcons/clones.git")
  :description "A work-in-progress Emulator for the Nintendo Entertainment System."
  :depends-on (:alexandria :serapeum :mgl-pax)
  :components ((:module "src"
                :components
                ((:file "clones"))))
  :in-order-to ((test-op (test-op clones/test))))

(defsystem clones/test
  :version "0.1"
  :author "Brit Butler"
  :license "MIT"
  :description "Tests for Clones."
  :depends-on (:clones :try)
  :components ((:module "test"
                :serial t
                :components ((:file "tests"))))
  :perform (test-op (o s)
             (uiop:symbol-call '#:clones-test '#:test)))
