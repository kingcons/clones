(defpackage :clones.test.render
  (:use :cl :clones.render :try)
  (:export #:test-render))

(in-package :clones.test.render)

(deftest test-render ()
  )

#+nil
(try 'test-render)
