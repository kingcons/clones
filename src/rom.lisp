(in-package :cl-user)

(defpackage :clones.rom
  (:use :cl :clones.util :clones.conditions)
  (:import-from :alexandria :read-file-into-byte-vector
                            :define-constant)
  (:export #:parse-rom
           #:rom
           #:rom-pathname
           #:rom-header
           #:rom-prg
           #:rom-chr
           #:rom-prg-count
           #:rom-chr-count
           #:rom-mirroring
           #:rom-mapper-name))

(in-package :clones.rom)

;;;; References:
;;;; http://fms.komkon.org/EMUL8/NES.html#LABM

(define-constant +mappers+
  '((0  . :nrom)    ; All 32kB ROM + 8kB VROM games, SMB
    (1  . :mmc1)    ; Final Fantasy, Metroid, Mega Man 2, Zelda
    (2  . :unrom)   ; Castlevania, Contra, Metal Gear, Mega Man
    (3  . :cnrom)   ; Cybernoid, Gradius, PipeDream, QBert
    (4  . :mmc3)    ; Double Dragon II, SMB 3, SuperContra
    (5  . :mmc5))   ; Castlevania3
  :test #'equal
  :documentation "An association list of the most common NES memory mappers.")

(defstruct rom
  (pathname    nil :read-only t)
  (prg         nil :read-only t)
  (chr         nil :read-only t)
  (prg-count   nil :read-only t)
  (chr-count   nil :read-only t)
  (mirroring   nil :read-only t)
  (mapper-name nil :read-only t))

(defmethod print-object ((obj rom) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (pathname prg-count chr-count mapper-name) obj
      (format stream "~A.~A :prg-size ~D :chr-size ~D :mapper-name ~A"
              (pathname-name pathname)
              (pathname-type pathname)
              (* prg-count #x4000)
              (* chr-count #x2000)
              mapper-name))))

(defun parse-rom (pathname)
  (let* ((bytes (read-file-into-byte-vector pathname))
         (metadata (parse-header pathname (subseq bytes 0 16)))
         (prg-size (getf metadata :prg-size))
         (prg (subseq bytes 16 (+ 16 prg-size)))
         (chr (subseq bytes (+ 16 prg-size))))
    (make-rom :pathname pathname
              :prg prg
              :chr chr
              :prg-count (getf metadata :prg-count)
              :chr-count (getf metadata :chr-count)
              :mirroring (getf metadata :mirroring)
              :mapper-name (getf metadata :mapper-name))))

(defun parse-header (pathname header)
  (unless (valid-header-p header)
    (error 'invalid-header header :file pathname :header header))
  (let ((mapper-id (%mapper-id header))
        (prg-count (aref header 4))
        (chr-count (aref header 5)))
    (list :prg-count   prg-count
          :prg-size    (* #x4000 prg-count)
          :chr-count   chr-count
          :chr-size    (* #x2000 chr-count)
          :mapper-id   mapper-id
          :mapper-name (%mapper-name mapper-id)
          :mirroring   (if (zerop (ldb (byte 1 0) (aref header 6)))
                           :horizontal
                           :vertical))))

(defun valid-header-p (bytes)
  "Ensure the first 4 bytes are NES-ctrl-Z and bytes 10-16 are 0. (see: iNES spec)"
  (and (string= "NES" (map 'string #'code-char (subseq bytes 0 4)))
       (every #'zerop (subseq bytes 10 16))))

(defun %mapper-id (bytes)
  "Use the high bits in bytes 6 and 7 to compute the Mapper ID."
  (let ((low-bits (ldb (byte 4 4) (aref bytes 6)))
        (high-bits (ldb (byte 4 4) (aref bytes 7))))
    (+ (ash high-bits 4) low-bits)))

(defun %mapper-name (id)
  "Look up the mapper name associated with ID in +MAPPERS+."
  (or (rest (assoc id +mappers+)) "Unknown"))
