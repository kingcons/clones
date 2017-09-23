(in-package :cl-user)

(defpackage :clones.util
  (:use :cl)
  (:export #:asset-path
           #:ub8
           #:ub16
           #:byte-vector
           #:make-byte-vector
           #:wrap-byte
           #:wrap-word
           #:enable-sharpf-read-macro))

(in-package :clones.util)

(defun enable-sharpf-read-macro ()
  "Add a read macro #f to easily declare optimization settings even in defmacro.
   From Let Over Lambda, this is especially useful for an emulator such as clones."
  (set-dispatch-macro-character #\# #\f
                                (lambda (stream sub-char numarg)
                                  (declare (ignore stream sub-char))
                                  (setf numarg (or numarg 3))
                                  (unless (<= numarg 3)
                                    (error "Invalid value for optimize declaration: ~a" numarg))
                                  `(declare (optimize (speed ,numarg)
                                                      (safety ,(- 3 numarg)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-sharpf-read-macro))

(defun asset-path (namestring)
  "Compute the relative path of a static asset in the clones project."
  (asdf:system-relative-pathname :clones namestring))

(deftype ub8 () '(unsigned-byte 8))
(deftype ub16 () '(unsigned-byte 16))
(deftype byte-vector (&optional (length '*))
  `(simple-array ub8 ,length))

(defun make-byte-vector (size)
  "Make a byte vector of length SIZE."
  (make-array size :element-type 'ub8))

(declaim (inline wrap-byte))
(defun wrap-byte (number)
  "Constrain a number to (integer 0 255)."
  #f
  (logand number #xff))

(declaim (inline wrap-word))
(defun wrap-word (number)
  "Constrain a number to (integer 0 65535)."
  #f
  (logand number #xffff))
