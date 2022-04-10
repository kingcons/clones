(mgl-pax:define-package :clones.util
  (:use :cl :mgl-pax))

(in-package :clones.util)

(defsection @util (:title "Assorted Utilities")
  (wrap-byte function)
  (wrap-word function))

(defun wrap-byte (value)
  (declare (fixnum value))
  (ldb (byte 8 0) value))

(defun wrap-word (value)
  (declare (fixnum value))
  (ldb (byte 16 0) value))
