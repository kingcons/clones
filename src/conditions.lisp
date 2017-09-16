(in-package :cl-user)

(defpackage :clones.conditions
  (:use :cl)
  (:export #:clones-error
           #:invalid-rom))

(in-package :clones.conditions)

(define-condition clones-error (error)
  ()
  (:documentation "The base condition for all errors in CLONES."))

(define-condition invalid-rom (clones-error)
  ((file :initarg :file :reader file)
   (header :initarg :header :reader header))
  (:report (lambda (condition stream)
             (format stream "Could not parse the ROM at ~A.~%~%Header was ~A.~%"
                     (file condition) (header condition)))))
