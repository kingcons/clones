(in-package :cl-user)

(defpackage :clones.util
  (:use :cl)
  (:export #:clones-asset
           #:byte-vector))

(in-package :clones.util)

(deftype ub8 () '(unsigned-byte 8))
(deftype byte-vector () '(simple-array ub8 (*)))

(defun clones-asset (namestring)
  "Compute the relative path of a static asset in the clones project."
  (asdf:system-relative-pathname :clones namestring))
