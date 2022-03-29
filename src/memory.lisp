(mgl-pax:define-package :clones.memory
  (:use :cl :alexandria :mgl-pax)
  (:import-from :serapeum
                #:make-octet-vector
                #:octet-vector)
  (:import-from :clones.mappers
                #:mapper
                #:get-prg
                #:set-prg
                #:load-rom))

(in-package :clones.memory)

(defsection @memory (:title "Memory Interface")
  (memory structure)
  (make-memory function)
  (fetch function)
  (store function))

(defstruct memory
  (ram (make-octet-vector #x800) :type octet-vector)
  (cart (load-rom (asdf:system-relative-pathname :clones "roms/nestest.nes")) :type mapper))

(defun fetch (memory address)
  (with-slots (ram cart) memory
    (cond ((< address #x2000) ;; read from RAM
           (aref ram (logand address #x7ff)))
          ((< address #x8000) ;; read from peripherals (ppu, apu, input)
           0)
          (t ;; read from cartridge
           (get-prg cart address)))))

(defun store (memory address value)
  (with-slots (ram cart) memory
    (cond ((< address #x2000) ;; write to RAM
           (setf (aref ram (logand address #x7ff)) value))
          ((< address #x8000) ;; write to peripherals (ppu, apu, input)
           nil)
          (t ;; write to cartridge
           (set-prg cart address value)))))
