#|
  This file is a part of clones project.
  Copyright (c) 2017 Brit Butler (brit@kingcons.io)
|#

(in-package :cl-user)
(defpackage clones-test-asd
  (:use :cl :asdf))
(in-package :clones-test-asd)

(defsystem clones-test
  :author "Brit Butler"
  :license "LLGPL"
  :depends-on (:clones
               :prove)
  :components ((:module "t"
                :components
                ((:file "helpers")
                 (:test-file "cpu")
                 (:test-file "addressing")
                 (:test-file "memory")
                 (:test-file "mappers")
                 (:test-file "rom"))))
  :description "Test system for clones"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
