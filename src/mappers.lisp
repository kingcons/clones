(in-package :cl-user)

(defpackage :clones.mappers
  (:use :cl :clones.rom)
  (:import-from :clones.conditions
                :unsupported-mapper)
  (:import-from :clones.util
                :ub16
                :ub8
                :wrap-byte
                :wrap-prg
                :wrap-chr)
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
      (:nrom  (make-nrom :rom rom))
      (:mmc1  (make-mmc1 :rom rom))
      (:unrom (make-unrom :rom rom))
      (otherwise (error 'unsupported-mapper :mapper-name mapper :rom rom)))))

;;; End of Mapper Protocol, Concrete implementations follow...

(defstruct mapper rom)

;;; Mapper 0 - NROM

(defstruct (nrom (:include mapper)))

(defmethod load-prg ((mapper nrom) address)
  (let* ((rom (mapper-rom mapper))
         (end-of-rom (1- (rom-prg-size rom)))
         (wrapped-address (logand address end-of-rom)))
    (aref (rom-prg rom) wrapped-address)))

(defmethod store-prg ((mapper nrom) address value)
  (declare (ignore address value))
  0)

(defmethod load-chr ((mapper nrom) address)
  (let ((rom (mapper-rom mapper)))
    (aref (rom-chr rom) address)))

(defmethod store-chr ((mapper nrom) address value)
  (let ((rom (mapper-rom mapper)))
    (setf (aref (rom-chr rom) address) value)))

;;; Mapper 1 - MMC1

(defstruct (mmc1 (:include mapper))
  ;; TODO: Implement PRG RAM of 8kb from 0x6000 -> 0x8000.
  (prg-bank    0    :type ub8)
  (chr-bank-1  0    :type ub8)
  (chr-bank-2  0    :type ub8)
  (write-count 0    :type ub8)
  (accumulator 0    :type ub8)
  (mirroring   :lower      :type keyword)
  (chr-mode    :switch-one :type keyword)
  (prg-mode    :switch-low :type keyword))

(defmethod reset ((mapper mmc1))
  (with-slots (accumulator write-count chr-mode prg-mode mirroring) mapper
    (setf chr-mode  :switch-one
          prg-mode  :switch-low
          mirroring :lower
          accumulator 0
          write-count 0)))

(defmethod set-modes ((mapper mmc1) value)
  (with-slots (mirroring chr-mode prg-mode) mapper
    (let ((new-mirror
            (case (ldb (byte 2 0) value)
              (0 :lower)
              (1 :upper)
              (2 :vertical)
              (3 :horizontal)))
          (new-prg-mode
            (case (ldb (byte 2 2) value)
              ((0 1) :switch-both)
              (2     :switch-high)
              (3     :switch-low)))
          (new-chr-mode
            (case (ldb (byte 1 4) value)
              (0 :switch-one)
              (1 :switch-two))))
      (setf mirroring new-mirror
            prg-mode new-prg-mode
            chr-mode new-chr-mode))))

(defmethod update-register ((mapper mmc1) address)
  (with-slots (accumulator write-count prg-bank chr-bank-1 chr-bank-2 rom) mapper
    (let* ((chr-count (rom-chr-count rom))
           (chr-bank (if (< chr-count 3)
                         (logand accumulator 1)
                         accumulator)))
      (cond ((< address #xA000)
             (set-modes mapper accumulator))
            ((< address #xC000)
             (setf chr-bank-1 chr-bank))
            ((< address #xE000)
             (setf chr-bank-2 chr-bank))
            (t
             (setf prg-bank accumulator))))
    (setf accumulator 0
          write-count 0)))

(defmethod load-prg ((mapper mmc1) address)
  (with-accessors ((prg-mode mmc1-prg-mode)
                   (prg-bank mmc1-prg-bank)
                   (rom mapper-rom)) mapper
    (flet ((get-low-bank ()
             (case prg-mode
               (:switch-both (error 'not-yet-implemented))
               (:switch-low prg-bank)
               (:switch-high 0)))
           (get-high-bank ()
             (case prg-mode
               (:switch-both (error 'not-yet-implemented))
               (:switch-low (1- (rom-prg-count rom)))
               (:switch-high prg-bank))))
      (let* ((bank (if (< address #xC000)
                       (get-low-bank)
                       (get-high-bank)))
             (bank-offset (* #x4000 bank)))
        (aref (rom-prg rom) (+ bank-offset (wrap-prg address)))))))

(defmethod store-prg ((mapper mmc1) address value)
  (if (logbitp 7 value)
      (reset mapper)
      (with-slots (accumulator write-count) mapper
        (setf (ldb (byte 1 write-count) accumulator) (logand value 1))
        (incf write-count)
        (when (= write-count 5)
          (update-register mapper address)))))

(defmethod load-chr ((mapper mmc1) address)
  (with-accessors ((chr-bank-1 mmc1-chr-bank-1)
                   (chr-bank-2 mmc1-chr-bank-2)
                   (rom mapper-rom)) mapper
    (let ((bank (if (< address #x1000) chr-bank-1 chr-bank-2)))
      (aref (rom-chr rom) (+ (* bank #x1000) (wrap-chr address))))))

(defmethod store-chr ((mapper mmc1) address value)
  (with-accessors ((chr-bank-1 mmc1-chr-bank-1)
                   (chr-bank-2 mmc1-chr-bank-2)
                   (rom mapper-rom)) mapper
    (let ((bank (if (< address #x1000) chr-bank-1 chr-bank-2)))
      (setf (aref (rom-chr rom) (+ (* bank #x1000) (wrap-chr address))) value))))

;;; Mapper 2 - UNROM

(defstruct (unrom (:include mapper))
  (switched-bank 0 :type ub8))

(defmethod load-prg ((mapper unrom) address)
  (with-accessors ((switched-bank unrom-switched-bank) (rom mapper-rom)) mapper
    (let* ((bank (if (< address #xC000)
                     switched-bank
                     (1- (rom-prg-count rom))))
           (bank-offset (* bank #x4000)))
      (aref (rom-prg rom) (+ bank-offset (wrap-prg address))))))

(defmethod store-prg ((mapper unrom) address value)
  (declare (ignore address))
  (setf (unrom-switched-bank mapper) (wrap-byte value)))

(defmethod load-chr ((mapper unrom) address)
  (let ((rom (mapper-rom mapper)))
    (aref (rom-chr rom) address)))

(defmethod store-chr ((mapper unrom) address value)
  (let ((rom (mapper-rom mapper)))
    (setf (aref (rom-chr rom) address) value)))
