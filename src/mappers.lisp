(in-package :cl-user)

(defpackage :clones.mappers
  (:use :cl :clones.rom)
  (:import-from :clones.conditions
                :unsupported-mapper)
  (:import-from :clones.util
                :ub16
                :enable-sharpf-read-macro)
  (:export #:mapper
           #:load-prg
           #:store-prg
           #:load-chr
           #:store-chr
           #:load-rom))

(in-package :clones.mappers)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-sharpf-read-macro))

;;; Mapper Protocol Notes:
;;; store and store-chr are permitted to return an arbitrary value if
;;; a particular mapper does not support the store operation.
;;; mapper-p is created by defstruct but unclear if we want to export it.

(defgeneric load-prg (mapper address)
  (:documentation "Load the value at ADDRESS from MAPPER."))

(defgeneric store-prg (mapper address value)
  (:documentation "Store VALUE into MAPPER at the specified ADDRESS."))

(defgeneric load-chr (mapper address)
  (:documentation "Load the value at ADDRESS from the CHR banks of MAPPER."))

(defgeneric store-chr (mapper address value)
  (:documentation "Store VALUE into the CHR banks of MAPPER at ADDRESS."))

(defun load-rom (pathname)
  "Builds a Mapper object from PATHNAME, raising conditions if:
    * The file at PATHNAME does not exist
    * The iNES header of the file is invalid
    * The Mapper used by the rom is unsupported."
  (unless (probe-file pathname)
    (error 'file-error :pathname pathname))
  (let* ((rom (parse-rom pathname))
         (mapper (rom-mapper-name rom)))
    (case mapper
      (:nrom (make-nrom :rom rom))
      (otherwise (error 'unsupported-mapper :mapper-name mapper :rom rom)))))

;;; End of Mapper Protocol, Concrete implementations follow...

(defstruct mapper rom)

(defstruct (nrom (:include mapper)))

(defmethod load-prg ((mapper nrom) address)
  #f
  (declare (type ub16 address))
  (let* ((rom (mapper-rom mapper))
         (end-of-rom (1- (rom-prg-size rom)))
         (wrapped-address (logand address end-of-rom)))
    (aref (rom-prg rom) wrapped-address)))

(defmethod store-prg ((mapper nrom) address value)
  (declare (ignore address value))
  0)

(defmethod load-chr ((mapper nrom) address)
  #f
  (declare (type ub16 address))
  (let ((rom (mapper-rom mapper)))
    (aref (rom-chr rom) address)))

(defmethod store-chr ((mapper nrom) address value)
  (declare (ignore address value))
  0)
