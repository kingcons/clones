(in-package :cl-user)

(defpackage :clones.memory
  (:use :cl :clones.mappers)
  (:import-from :clones.mappers
                :mapper
                :load-rom)
  (:import-from :clones.util
                :asset-path
                :ub8
                :ub16
                :byte-vector
                :make-byte-vector)
  (:export #:memory
           #:make-memory
           #:fetch
           #:store))

(in-package :clones.memory)

(defstruct memory
  (ram (make-byte-vector #x800) :type byte-vector)
  (ppu nil)
  (apu nil)
  (mapper (load-rom (asset-path "roms/nestest.nes")) :type mapper))

(declaim (ftype (function (memory ub16) ub8) fetch))
(defun fetch (memory address)
  #f
  (cond ((< address #x2000)
         (aref (memory-ram memory) (logand address #x7ff)))
;        ((< address #x4000) 0) ; TODO: PPU Not Yet Implemented
;        ((= address #x4016) 0) ; TODO: Input Not Yet Implemented
;        ((< address #x4019) 0) ; TODO: APU Not Yet Implemented
;        ((< address #x8000) 0) ; TODO: SRAM Not Yet Implemented
        (t
         (load-prg (memory-mapper memory) address))))

(declaim (ftype (function (memory ub16 ub8) ub8) store))
(defun store (memory address value)
  #f
  (cond ((< address #x2000)
         (setf (aref (memory-ram memory) (logand address #x7ff)) value))
;        ((< address #x4000) 0) ; TODO: PPU Not Yet Implemented
;        ((= address #x4016) 0) ; TODO: Input Not Yet Implemented
;        ((< address #x4019) 0) ; TODO: APU Not Yet Implemented
;        ((< address #x8000) 0) ; TODO: SRAM Not Yet Implemented
        (t
         (store-prg (memory-mapper memory) address value))))
