(mgl-pax:define-package :clones.mappers
  (:use :cl :alexandria :mgl-pax)
  (:import-from :clones.rom #:parse-rom))

(in-package :clones.mappers)

(defsection @mappers (:title "Mapper Interface")
  (mapper class)
  (load-rom function)
  (unimplemented-mapper condition)
  (get-prg generic-function)
  (set-prg generic-function)
  (get-chr generic-function)
  (set-chr generic-function))

(defclass mapper ()
  ((pathname :initarg :pathname :reader mapper-pathname)
   (prg-count :initarg :prg-count :reader prg-count)
   (chr-count :initarg :chr-count :reader chr-count)
   (prg :initarg :prg :type (simple-array (unsigned-byte 8) *))
   (chr :initarg :chr :type (simple-array (unsigned-byte 8) *)))
  (:documentation "A Mapper is a virtual representation of a game cartridge,
referenced by the PPU for purposes of accessing graphical data (CHR) and by the
CPU for purposes of accessing program code (PRG)."))

(define-condition unimplemented-mapper ()
  ((pathname :initarg :pathname :reader mapper-pathname)
   (mapper-name :initarg :mapper-name :reader mapper-name))
  (:report (lambda (condition stream)
             (format stream "The file at ~S uses a mapper Clones has not yet implemented: ~A"
                     (mapper-pathname condition)
                     (mapper-name condition))))
  (:documentation "Signalled when no subclass of MAPPER implements the
cartridge for the file supplied to LOAD-ROM."))

(defun load-rom (&optional (pathname (asdf:system-relative-pathname :clones "roms/nestest.nes")))
  "Given a PATHNAME to a valid Nintendo ROM, process the file using PARSE-ROM
and return an appropriate instance of MAPPER for the cartridge type of the game.
An UNIMPLEMENTED-MAPPER condition will be signalled if the cartridge type is not
yet supported by clones. If no PATHNAME is supplied, the NEStest ROM will be used."
  (let* ((mapper-args (parse-rom pathname))
         (mapper-name (getf mapper-args :mapper-name)))
    (remove-from-plistf mapper-args :mirroring :mapper-name)
    (case mapper-name
      (:nrom (apply 'make-instance 'nrom mapper-args))
      (otherwise (error 'unimplemented-mapper :pathname pathname :mapper-name mapper-name)))))

(defgeneric get-prg (mapper address)
  (:documentation "Retrieve the value at ADDRESS from the PRG bank of MAPPER."))

(defgeneric set-prg (mapper address value)
  (:documentation "Set ADDRESS in the PRG bank of MAPPER to VALUE."))

(defgeneric get-chr (mapper address)
  (:documentation "Retrive the value at ADDRESS from the CHR bank of MAPPER."))

(defgeneric set-chr (mapper address value)
  (:documentation "Set ADDRESS in the CHR bank of MAPPER to VALUE."))

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
