(in-package :cl-user)

(defpackage :clones.util
  (:use :cl)
  (:export #:*standard-optimize-settings*
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
           #:wrap-prg
           #:wrap-chr
           #:flip-bit
           #:page-crossed-p
           #:slot->))

(in-package :clones.util)

(defvar *standard-optimize-settings*
  '(optimize speed (debug 1) (space 0) (compilation-speed 0))
  "Optimize settings to use in opcode definitions (where safety 0 won't do).")

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

(defun wrap-byte (number)
  "Constrain a number to (integer 0 255)."
  (logand number #xff))

(defun wrap-word (number)
  "Constrain a number to (integer 0 65535)."
  (logand number #xffff))

(defun wrap-nametable (number)
  "Constrain a number to (integer 0 2047)."
  (logand number #x7ff))

(defun wrap-palette-table (number)
  "Constrain a number to (integer 0 31)."
  (logand number #x1f))

(defun wrap-palette (number)
  "Constrain a number to (integer 0 63)."
  (logand number #x3f))

(defun wrap-prg (number)
  "Constrain a number to (integer 0 16383)."
  (logand number #x3fff))

(defun wrap-chr (number)
  "Constrain a number to (integer 0 4095)."
  (logand number #xfff))

(defmacro flip-bit (position value)
  `(logxor ,(expt 2 position) ,value))

(defun page-crossed-p (start final)
  (/= (logand start #xff00)
      (logand final #xff00)))

(defmacro slot-> (object &rest slots)
  (if (null slots)
      object
      `(slot-> (slot-value ,object ',(car slots)) ,@(rest slots))))
