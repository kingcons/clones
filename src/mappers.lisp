(mgl-pax:define-package :clones.mappers
  (:use :cl :alexandria :mgl-pax)
  (:import-from :clones.rom
                #:parse-rom)
  (:import-from :serapeum
                #:octet
                #:octet-vector))

(in-package :clones.mappers)

(defsection @mappers (:title "Mapper Interface")
  (mapper class)
  (load-rom function)
  (unimplemented-mapper condition)
  (mapper-pathname generic-function)
  (get-prg generic-function)
  (set-prg generic-function)
  (get-chr generic-function)
  (set-chr generic-function)
  (mirroring (accessor mapper)))

(defclass mapper ()
  ((pathname :initarg :pathname :reader mapper-pathname)
   (prg-count :initarg :prg-count :reader prg-count)
   (chr-count :initarg :chr-count :reader chr-count)
   (prg :initarg :prg :type octet-vector)
   (chr :initarg :chr :type octet-vector)
   (mirroring :initarg :mirroring :accessor mirroring))
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
    (remove-from-plistf mapper-args :mapper-name)
    (case mapper-name
      (:nrom (apply 'make-instance 'nrom mapper-args))
      (:mmc1 (apply 'make-instance 'mmc1 mapper-args))
      (otherwise (error 'unimplemented-mapper :pathname pathname :mapper-name mapper-name)))))

(defgeneric get-prg (mapper address)
  (:documentation "Retrieve the value at ADDRESS from the PRG bank of MAPPER."))

(defgeneric set-prg (mapper address value)
  (:documentation "Set ADDRESS in the PRG bank of MAPPER to VALUE."))

(defgeneric get-chr (mapper address)
  (:documentation "Retrive the value at ADDRESS from the CHR bank of MAPPER."))

(defgeneric set-chr (mapper address value)
  (:documentation "Set ADDRESS in the CHR bank of MAPPER to VALUE."))

;;; Mapper 0 - NROM

(defclass nrom (mapper) ())

(defmethod get-prg ((mapper nrom) address)
  (with-slots (prg) mapper
    (aref prg (logand address (1- (length prg))))))

(defmethod set-prg ((mapper nrom) address value)
  (declare (ignore address value))
  nil)

(defmethod get-chr ((mapper nrom) address)
  (with-slots (chr) mapper
    (aref chr (logand address #x1FFF))))

(defmethod set-chr ((mapper nrom) address value)
  (declare (ignore address value))
  nil)

;;; Mapper 1 - MMC1

(defclass mmc1 (mapper)
  ((prg-mode :initform :low :type symbol :accessor prg-mode)
   (chr-mode :initform :one :type symbol :accessor chr-mode)
   (prg-bank :initform 0 :type octet :accessor prg-bank)
   (chr-bank1 :initform 0 :type octet :accessor chr-bank1)
   (chr-bank2 :initform 0 :type octet :accessor chr-bank2)
   (write-count :initform 0 :type octet :accessor write-count)
   (accumulator :initform 0 :type octet :accessor accumulator)))

(defun choose-mirroring (accumulator)
  (case (ldb (byte 0 2) accumulator)
    (3 :horizontal)
    (2 :vertical)
    (1 :upper)
    (0 :lower)))

(defun choose-prg-mode (accumulator)
  (case (ldb (byte 2 2) accumulator)
    (3 :high)
    (2 :low)
    (t :both)))

(defun choose-chr-mode (accumulator)
  (if (logbitp 5 accumulator)
      :one
      :two))

(defgeneric reconfigure (mapper address)
  (:documentation "Reconfigure the MAPPER based upon the value of ADDRESS."))

(defmethod reconfigure ((mapper mmc1) address)
  (let ((accum (accumulator mapper)))
    (case (ldb (byte 2 13) address)
      (0
       (setf (mirroring mapper) (choose-mirroring accum)
             (prg-mode mapper) (choose-prg-mode accum)
             (chr-mode mapper) (choose-chr-mode accum)))
      (1
       (if (eql :one (chr-mode mapper))
           (setf (chr-bank1 mapper) accum)
           (setf (chr-bank1 mapper) accum
                 (chr-bank2 mapper) (logior accum 1))))
      (2
       (when (eql :one (chr-mode mapper))
         (setf (chr-bank2 mapper) accum)))
      (t
       (setf (prg-bank mapper) accum)))))

(defun chr-address (mapper offset)
  (let ((bank (if (< offset #x1000)
                  (chr-bank1 mapper)
                  (chr-bank2 mapper))))
    (+ (* bank #x1000)
       (logand offset #xfff))))

(defun prg-address (mapper offset)
  (let ((bank (if (< offset #xc000)
                  (prg-bank mapper)
                  (1- (prg-count mapper)))))
    (+ (* bank #x4000)
       (logand offset #x3fff))))

(defmethod get-prg ((mapper mmc1) address)
  (with-slots (prg) mapper
    (aref prg (prg-address mapper address))))

(defmethod set-prg ((mapper mmc1) address value)
  (with-slots (accumulator write-count) mapper
    (flet ((reset ()
             (setf accumulator 0
                   write-count 0)))
      (if (logbitp 7 value)
          (reset)
          (let ((bit (logand value 1)))
            (setf accumulator (dpb bit (byte 1 write-count) accumulator)
                  write-count (1+ write-count))
            (when (= write-count 5)
              (reconfigure mapper address)
              (reset)))))))

(defmethod get-chr ((mapper mmc1) address)
  (with-slots (chr) mapper
    (aref chr (chr-address mapper address))))

(defmethod set-chr ((mapper mmc1) address value)
  (with-slots (chr) mapper
    (setf (aref chr (chr-address mapper address)) value)))

;;; Mapper 2 - UNROM

;;; Mapper 3 - CNROM

;;; Mapper 4 - MMC3
