(in-package :cl-user)

(defpackage :clones.mappers
  (:use :cl :clones.rom)
  (:import-from :clones.conditions
                :unsupported-mapper)
  (:import-from :clones.util
                :ub8
                :ub16
                :wrap-bank
                :wrap-nibble)
  (:export #:mapper
           #:load-prg
           #:store-prg
           #:load-chr
           #:store-chr
           #:load-rom))

(in-package :clones.mappers)

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
      (:mmc1 (make-mmc1 :rom rom))
      (:unrom (make-unrom :rom rom))
      (:cnrom (make-cnrom :rom rom))
      (otherwise (error 'unsupported-mapper :mapper-name mapper :rom rom)))))

;;; End of Mapper Protocol, Concrete implementations follow...

(defstruct mapper rom)

;;; NROM

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

;;; MMC1

(defstruct (mmc1 (:include mapper)))

(defmethod load-prg ((mapper mmc1) address))

(defmethod store-prg ((mapper mmc1) address value))

(defmethod load-chr ((mapper mmc1) address))

(defmethod store-chr ((mapper mmc1) address value))

;;; UNROM

(defstruct (unrom (:include mapper))
  (prg-bank 0 :type ub8))

(defmethod load-prg ((mapper unrom) address)
  #f
  (declare (type ub16 address))
  (let* ((rom (mapper-rom mapper))
         (prg-bank (if (< address #xC000)
                       (unrom-prg-bank mapper)
                       (1- (rom-prg-count rom))))
         (prg-offset (* prg-bank #x4000)))
    (aref (rom-prg rom) (+ prg-offset (wrap-bank address)))))

(defmethod store-prg ((mapper unrom) address value)
  (declare (ignore address))
  (setf (unrom-prg-bank mapper) (wrap-nibble value)))

(defmethod load-chr ((mapper unrom) address)
  #f
  (declare (type ub16 address))
  (let ((rom (mapper-rom mapper)))
    (aref (rom-chr rom) address)))

(defmethod store-chr ((mapper unrom) address value)
  (declare (ignore address value))
  0)

;;; CNROM

(defstruct (cnrom (:include mapper))
  (chr-bank 0 :type ub8))

(defmethod load-prg ((mapper cnrom) address)
  #f
  (declare (type ub16 address))
  (let* ((rom (mapper-rom mapper))
         (end-of-rom (1- (rom-prg-size rom)))
         (wrapped-address (logand address end-of-rom)))
    (aref (rom-prg rom) wrapped-address)))

(defmethod store-prg ((mapper cnrom) address value)
  (declare (type ub16 address))
  (setf (cnrom-chr-bank mapper) (wrap-nibble value)))

(defmethod load-chr ((mapper cnrom) address)
  )

(defmethod store-chr ((mapper cnrom) address value)
  )
