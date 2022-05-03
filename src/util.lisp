(mgl-pax:define-package :clones.util
  (:use :cl :mgl-pax))

(in-package :clones.util)

(defsection @util (:title "Assorted Utilities")
  (wrap-byte function)
  (wrap-word function)
  (define-printer macro))

(defun wrap-byte (value)
  (declare (fixnum value))
  (ldb (byte 8 0) value))

(defun wrap-word (value)
  (declare (fixnum value))
  (ldb (byte 16 0) value))

(defmacro define-printer (type (&rest vars) format-string &rest args)
  "Define printer is a helper macro for generating a PRINT-OBJECT
method. DEFINE-PRINTER provides a shorthand for the common case where
slot values need to be safely displayed but not read back in."
  `(defmethod print-object ((,type ,type) stream)
     (let ,(loop for v in vars
                 collect `(,v (handler-case (slot-value ,type ',v)
                                (unbound-slot () :unbound))))
       (print-unreadable-object (,type stream :type t)
         (let ((*print-pretty* nil))
           (format stream ,format-string ,@args))))))
