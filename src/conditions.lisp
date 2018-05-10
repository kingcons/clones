(in-package :cl-user)

(defpackage :clones.conditions
  (:use :cl)
  (:export #:clones-error
           #:invalid-rom
           #:unsupported-mapper
           #:illegal-opcode))

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

(define-condition unsupported-mapper (clones-error)
  ((rom :initarg :rom :reader rom)
   (mapper-name :initarg :mapper-name :reader mapper-name))
  (:report (lambda (condition stream)
             (format stream "The mapper ~S is not currently supported.~%~%"
                     (mapper-name condition))
             (format stream "ROM: ~A~%~%" (rom condition)))))

(define-condition illegal-opcode (clones-error)
  ((cpu :initarg :cpu :reader cpu)
   (opcode :initarg :opcode :reader opcode))
  (:report (lambda (condition stream)
             (format stream "Could not execute the requested opcode (~X) for ~A.~%"
                     (opcode condition)
                     (cpu condition)))))
