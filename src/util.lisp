(in-package :cl-user)

(defpackage :clones.util
  (:use :cl)
  (:export #:*standard-optimize-settings*
           #:enable-sharpf-read-macro
           #:asset-path
           #:ub8
           #:ub16
           #:ub32
           #:byte-vector
           #:make-byte-vector
           #:wrap-byte
           #:wrap-word
           #:wrap-nametable
           #:wrap-palette-table
           #:wrap-palette
           #:wrap-bank
           #:flip-bit
           #:page-crossed-p
           #:slot->))

(in-package :clones.util)

(defvar *standard-optimize-settings*
  '(optimize speed (debug 1) (space 0) (compilation-speed 0))
  "Optimize settings to use in opcode definitions (where safety 0 won't do).")

(eval-when (:compile-toplevel :load-toplevel :execute)
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

  (enable-sharpf-read-macro))

(defun asset-path (namestring)
  "Compute the relative path of a static asset in the clones project."
  (asdf:system-relative-pathname :clones namestring))

(deftype ub8 () '(unsigned-byte 8))
(deftype ub16 () '(unsigned-byte 16))
(deftype ub32 () '(unsigned-byte 32))
(deftype byte-vector (&optional (length '*))
  `(simple-array ub8 ,(if (eq length '*) '* (list length))))

(defun make-byte-vector (size)
  "Make a byte vector of length SIZE."
  (make-array size :element-type 'ub8))

(declaim (inline wrap-byte))
(defun wrap-byte (number)
  "Constrain a number to (integer 0 255)."
  (logand number #xff))

(declaim (inline wrap-word))
(defun wrap-word (number)
  "Constrain a number to (integer 0 65535)."
  (logand number #xffff))

(declaim (inline wrap-nametable))
(defun wrap-nametable (number)
  "Constrain a number to (integer 0 2047)."
  (logand number #x7ff))

(declaim (inline wrap-palette-table))
(defun wrap-palette-table (number)
  "Constrain a number to (integer 0 31)."
  (logand number #x1f))

(declaim (inline wrap-palette))
(defun wrap-palette (number)
  "Constrain a number to (integer 0 63)."
  (logand number #x3f))

(declaim (inline wrap-bank))
(defun wrap-bank (number)
  "Constrain a number to (integer 0 16383)."
  (logand number #x3fff))

(defmacro flip-bit (position value)
  `(logxor ,(expt 2 position) ,value))

(declaim (inline page-crossed-p))
(declaim (ftype (function (ub16 ub16) boolean) page-crossed-p))
(defun page-crossed-p (start final)
  (/= (logand start #xff00)
      (logand final #xff00)))

(defmacro slot-> (object &rest slots)
  (if (null slots)
      object
      `(slot-> (slot-value ,object ',(car slots)) ,@(rest slots))))
