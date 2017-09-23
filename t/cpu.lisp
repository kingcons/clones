(in-package :cl-user)

(defpackage :clones-test.cpu
  (:use :cl :clones.cpu :prove))

(in-package :clones-test.cpu)

(plan 1)

(subtest "CPU Interface")

(finalize)
