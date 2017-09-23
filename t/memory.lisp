(in-package :cl-user)

(defpackage :clones-test.memory
  (:use :cl :clones.memory :prove))

(in-package :clones-test.memory)

(plan 1)

(subtest "Memory Interface")

(finalize)
