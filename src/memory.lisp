(mgl-pax:define-package :clones.memory
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.mappers :clones.ppu)
  (:import-from :serapeum
                #:make-octet-vector
                #:octet-vector))

(in-package :clones.memory)

(defsection @memory (:title "Memory Interface")
  (memory class)
  (make-memory function)
  (fetch function)
  (store function)
  (fetch-word function)
  (fetch-indirect function))

(defstruct memory
  (ram (make-octet-vector #x800) :type octet-vector)
  (ppu (make-ppu) :type ppu)
  (cart (load-rom (asdf:system-relative-pathname :clones "roms/nestest.nes")) :type mapper))

(defun fetch (memory address)
  (with-slots (ram ppu cart) memory
    (cond ((< address #x2000) ;; read from RAM
           (aref ram (logand address #x7ff)))
          ((< address #x4000) ;; read from PPU
           (read-ppu ppu (logand address #x7)))
          ((< address #x8000) ;; read from peripherals (ppu, apu, input)
           0)
          (t ;; read from cartridge
           (get-prg cart address)))))

(defun store (memory address value)
  (with-slots (ram ppu cart) memory
    (cond ((< address #x2000) ;; write to RAM
           (setf (aref ram (logand address #x7ff)) value))
          ((< address #x4000)
           (write-ppu ppu (logand address #x7) value))
          ((< address #x8000) ;; write to peripherals (ppu, apu, input)
           nil)
          (t ;; write to cartridge
           (set-prg cart address value)))))

(defun fetch-word (memory address)
  (let ((low-byte (fetch memory address))
        (high-byte (fetch memory (1+ address))))
    (dpb high-byte (byte 8 8) low-byte)))

(defun fetch-indirect (memory start)
  (flet ((wrap-page (value)
           (deposit-field start (byte 8 8) value)))
    (let ((low-byte (fetch memory start))
          (high-byte (fetch memory (wrap-page (1+ start)))))
      (dpb high-byte (byte 8 8) low-byte))))
