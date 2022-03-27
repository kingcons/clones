(mgl-pax:define-package :clones.rom
  (:use :cl :alexandria :serapeum :mgl-pax))

(in-package :clones.rom)

(defsection @rom (:title "ROM Parsing")
  (parse-rom function)
  (invalid-rom condition))

(defun valid-header? (stream)
  (let ((header #(78 69 83 26))
        (bytes (make-array 4)))
    (read-sequence bytes stream)
    (equalp bytes header)))

(defun get-mapper-name (low-byte high-byte)
  (let ((mapper-id (+ high-byte (ash low-byte -4))))
    (case mapper-id
      (0 :nrom)
      (1 :mmc1)
      (2 :unrom)
      (3 :cnrom)
      (4 :mmc3)
      (5 :mmc5)
      (otherwise :unsupported))))

(defun get-mirroring (ctrl-byte)
  (if (zerop (logand ctrl-byte 1))
      :horizontal
      :vertical))

(defun extract-metadata (stream)
  (let ((prg-count (read-byte stream))
        (chr-count (read-byte stream))
        (ctrl-byte-1 (read-byte stream))
        (ctrl-byte-2 (read-byte stream)))
    (list :prg-count prg-count
          :chr-count chr-count
          :mapper-name (get-mapper-name ctrl-byte-1 ctrl-byte-2)
          :mirroring (get-mirroring ctrl-byte-1))))

(define-condition invalid-rom (error)
  ((pathname :initarg :pathname :reader invalid-rom-pathname))
  (:report (lambda (condition stream)
             (format stream "The file at ~A is not a valid ROM."
                     (invalid-rom-pathname condition)))))

(defun parse-rom (pathname)
  (with-open-file (input pathname :element-type '(unsigned-byte 8))
    (unless (valid-header? input)
      (error 'invalid-rom :pathname pathname))
    (let* ((metadata (extract-metadata input))
           (prg-count (getf metadata :prg-count))
           (chr-count (getf metadata :chr-count))
           (prg (make-array (* #x4000 prg-count) :element-type '(unsigned-byte 8)))
           (chr (make-array (* #x2000 chr-count) :element-type '(unsigned-byte 8))))
      ;; Ignore zeroes in header.
      (dotimes (i 8)
        (read-byte input))
      (read-sequence prg input)
      (read-sequence chr input)
      (append metadata (list :prg prg :chr chr :pathname pathname)))))
