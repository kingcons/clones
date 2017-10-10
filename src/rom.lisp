(in-package :cl-user)

(defpackage :clones.rom
  (:use :cl :clones.util :clones.conditions)
  (:import-from :alexandria
                :read-file-into-byte-vector
                :define-constant)
  (:export #:parse-rom
           #:rom
           #:rom-pathname
           #:rom-header
           #:rom-prg
           #:rom-chr
           #:rom-prg-size
           #:rom-chr-size
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
    (4  . :mmc3))   ; Double Dragon II, SMB 3, SuperContra
  :test #'equal
  :documentation "An association list of the most common NES memory mappers.")

(defstruct rom
  (pathname    nil :read-only t :type pathname)
  (prg         #() :read-only t :type byte-vector)
  (chr         #() :read-only t :type byte-vector)
  (prg-size      0 :read-only t :type fixnum)
  (chr-size      0 :read-only t :type fixnum)
  (prg-count     0 :read-only t :type ub8)
  (chr-count     0 :read-only t :type ub8)
  (mirroring   nil :read-only t :type keyword)
  (mapper-name nil :read-only t :type keyword))

(defmethod print-object ((obj rom) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (pathname prg-size chr-size mapper-name) obj
      (format stream "~A.~A :prg-size ~D :chr-size ~D :mapper-name ~A"
              (pathname-name pathname)
              (pathname-type pathname)
              prg-size
              chr-size
              mapper-name))))

(defun parse-rom (pathname)
  (let* ((bytes (read-file-into-byte-vector pathname))
         (metadata (parse-header pathname (subseq bytes 0 16)))
         (prg-size (getf metadata :prg-size))
         (chr-size (getf metadata :chr-size))
         (prg (subseq bytes 16 (+ 16 prg-size)))
         (chr (subseq bytes (+ 16 prg-size))))
    (assert (= (length prg) prg-size))
    (assert (= (length chr) chr-size))
    (make-rom :pathname pathname
              :prg prg
              :chr chr
              :prg-size prg-size
              :chr-size chr-size
              :prg-count (getf metadata :prg-count)
              :chr-count (getf metadata :chr-count)
              :mirroring (getf metadata :mirroring)
              :mapper-name (getf metadata :mapper-name))))

(defun parse-header (pathname header)
  (unless (valid-header-p header)
    (error 'invalid-rom :file pathname :header header))
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
  (or (rest (assoc id +mappers+)) :unknown))
