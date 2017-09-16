(in-package :cl-user)

(defpackage :clones.util
  (:use :cl)
  (:export #:clones-asset))

(in-package :clones.util)

(defun clones-asset (namestring)
  "Compute the relative path of a static asset in the clones project."
  (asdf:system-relative-pathname :clones namestring))
