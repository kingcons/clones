(in-package :cl-user)

(defpackage :clones.memory
  (:use :cl :clones.mappers)
  (:import-from :clones.ppu
                :ppu
                :ppu-result
                :make-ppu
                :ppu-read
                :ppu-write)
  (:import-from :clones.mappers
                :mapper
                :mapper-rom
                :load-rom)
  (:import-from :clones.util
                :asset-path
                :ub8
                :ub16
                :byte-vector
                :make-byte-vector)
  (:export #:memory
           #:memory-ppu
           #:make-memory
           #:swap-rom
           #:fetch
           #:fetch-word
           #:fetch-indirect
           #:fetch-range
           #:store))

(in-package :clones.memory)

(defstruct memory
  (ram (make-byte-vector #x800) :type byte-vector)
  (ppu (make-ppu) :type ppu)
  (apu nil)
  (mapper (load-rom (asset-path "roms/nestest.nes")) :type mapper))

(declaim (inline %oam-dma))
(defun %oam-dma (memory value)
  (let ((ppu (memory-ppu memory))
        (page (ash value 8)))
    (loop for index from page to (+ page 256)
          do (let ((data (fetch memory index)))
               (ppu-write ppu #x2004 data)))
    (setf (getf (ppu-result ppu) :oam-dma) t)))

(defun swap-rom (memory rom-file)
  (let ((rom (load-rom (asset-path rom-file))))
    (setf (memory-mapper memory) rom)
    (clones.ppu:initialize-pattern-table (memory-ppu memory) (clones.mappers::mapper-rom rom))))

(declaim (ftype (function (memory ub16) ub8) fetch))
(defun fetch (memory address)
  #f
  (cond ((< address #x2000)
         (aref (memory-ram memory) (logand address #x7ff)))
        ((< address #x4000)
         (ppu-read (memory-ppu memory) address))
        ((< address #x8000)
         0)
        (t
         (load-prg (memory-mapper memory) address))))

(declaim (ftype (function (memory ub16 ub8) ub8) store))
(defun store (memory address value)
  #f
  (cond ((< address #x2000)
         (setf (aref (memory-ram memory) (logand address #x7ff)) value))
        ((< address #x4000)
         (ppu-write (memory-ppu memory) address value))
        ((= address #x4014)
         (%oam-dma memory value))
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
