(in-package :cl-user)

(defpackage :clones.util
  (:use :cl)
  (:export #:clones-asset
           #:ub8
           #:byte-vector
           #:make-byte-vector))

(in-package :clones.util)

(defun clones-asset (namestring)
  "Compute the relative path of a static asset in the clones project."
  (asdf:system-relative-pathname :clones namestring))

(deftype ub8 () '(unsigned-byte 8))
(deftype byte-vector (&optional (length '*))
  `(simple-array ub8 ,length))

(defun make-byte-vector (size)
  "Make a byte vector of length SIZE."
  (make-array size :element-type 'ub8))

