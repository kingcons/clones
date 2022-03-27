(mgl-pax:define-package :clones.mappers
  (:use :cl :alexandria :serapeum :mgl-pax))

(in-package :clones.mappers)

(defsection @mappers (:title "Mapper Interface")
  (load-rom function)
  (get-prg generic-function)
  (set-prg generic-function)
  (get-chr generic-function)
  (set-chr generic-function))

(defclass mapper ()
  ((pathname :initarg :pathname :reader mapper-pathname)
   (prg-count :initarg :prg-count :reader prg-count)
   (chr-count :initarg :chr-count :reader chr-count)
   (prg :initarg :prg :type (simple-array (unsigned-byte 8) *))
   (chr :initarg :chr :type (simple-array (unsigned-byte 8) *))))

(define-condition unsupported-mapper ()
  ((pathname :initarg :pathname :reader mapper-pathname))
  (:report (lambda (condition stream)
             (format stream "The file at ~S uses a mapper Clones will not support."
                     (mapper-pathname condition)))))

(define-condition unimplemented-mapper ()
  ((pathname :initarg :pathname :reader mapper-pathname)
   (mapper-name :initarg :mapper-name :reader mapper-name))
  (:report (lambda (condition stream)
             (format stream "The file at ~S uses a mapper Clones has not yet implemented: ~A"
                     (mapper-pathname condition)
                     (mapper-name condition)))))

(defun load-rom (pathname)
  "Given a pathname to a valid Nintendo ROM, load the ROM
and return an appropriate instance of MAPPER for the cartridge
type of the game."
  (let* ((mapper-args (clones.rom:parse-rom pathname))
         (mapper-name (getf mapper-args :mapper-name)))
    (when (eql :unsupported mapper-name)
      (error 'unsupported-mapper :pathname pathname))
    (alexandria:remove-from-plistf mapper-args :mirroring :mapper-name)
    (case mapper-name
      (:nrom (apply 'make-instance 'nrom mapper-args))
      (otherwise (error 'unimplemented-mapper :pathname pathname :mapper-name mapper-name)))))

(defgeneric get-prg (mapper address)
  (:documentation "Retrieve the value at ADDRESS from the prg bank."))

(defgeneric set-prg (mapper address value)
  (:documentation "Set ADDRESS in the prg bank to VALUE."))

(defgeneric get-chr (mapper address)
  (:documentation "Retrive the value at ADDRESS from the chr bank."))

(defgeneric set-chr (mapper address value)
  (:documentation "Set ADDRESS in the chr bank to VALUE."))

;; Mapper 0 - NROM

(defclass nrom (mapper) ())

(defmethod get-prg ((mapper nrom) address)
  (with-slots (prg) mapper
    (aref prg (logand address (1- (length prg))))))

(defmethod set-prg ((mapper nrom) address value)
  (declare (ignore address value))
  nil)

(defmethod get-chr ((mapper nrom) address)
  (with-slots (chr) mapper
    (aref chr (logand address (1- (length chr))))))

(defmethod set-chr ((mapper nrom) address value)
  (declare (ignore address value))
  nil)
