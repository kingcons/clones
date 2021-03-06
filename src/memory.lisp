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
           #:fetch-word
           #:fetch-indirect
           #:fetch-range
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
        ((< address #x8000)
         0)
        (t
         (load-prg (memory-mapper memory) address))))

(declaim (ftype (function (memory ub16 ub8) ub8) store))
(defun store (memory address value)
  #f
  (cond ((< address #x2000)
         (setf (aref (memory-ram memory) (logand address #x7ff)) value))
        ((< address #x8000)
         0)
        (t
         (store-prg (memory-mapper memory) address value))))

(declaim (ftype (function (memory ub16) ub16) fetch-word))
(defun fetch-word (memory address)
  #f
  (let ((low-byte  (fetch memory address))
        (high-byte (fetch memory (1+ address))))
    (+ low-byte (ash high-byte 8))))

(declaim (ftype (function (memory ub16) ub16) fetch-indirect))
(defun fetch-indirect (memory address)
  #f
  (let* ((wrapped (+ (logand address #xff00)
                     (logand (1+ address) #xff)))
         (low-byte (fetch memory address))
         (high-byte (fetch memory wrapped)))
    (+ low-byte (ash high-byte 8))))

(declaim (ftype (function (memory ub16 ub16) cons) fetch-range))
(defun fetch-range (memory start end)
  #f
  (loop for i from start upto end
        collect (fetch memory i)))
