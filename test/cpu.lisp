(defpackage :clones.test.cpu
  (:use :cl :clones.cpu :try)
  (:export #:test-cpu))

(in-package :clones.test.cpu)

(deftest test-cpu ()
  )

#+nil
(try 'test-cpu)
