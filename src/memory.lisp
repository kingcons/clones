(in-package :cl-user)

(defpackage :clones.memory
  (:use :cl :clones.mappers)
  (:import-from :clones.ppu
                :ppu
                :ppu-cartridge
                :ppu-dma-result
                :make-ppu)
  (:import-from :clones.input
                :gamepad
                :make-gamepad
                :fetch-strobe
                :reset-strobe)
  (:import-from :clones.util
                :asset-path
                :ub8
                :ub16
                :byte-vector
                :make-byte-vector)
  (:export #:memory
           #:memory-ppu
           #:memory-gamepad
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
  (gamepad (make-gamepad) :type gamepad)
  (mapper (load-rom (asset-path "roms/nestest.nes")) :type mapper))

(defun %oam-dma (memory value)
  (let ((ppu (memory-ppu memory))
        (page (ash value 8)))
    (loop for index from page to (+ page #xff)
          do (let ((data (fetch memory index)))
               (ppu-write ppu #x2004 data)))
    (setf (ppu-dma-result ppu) t)))

(defun swap-rom (memory rom-file)
  (let ((rom (load-rom (asset-path rom-file))))
    (with-accessors ((mapper memory-mapper) (ppu memory-ppu)) memory
      (setf mapper rom
            (ppu-cartridge ppu) rom))))

(defmethod fetch ((memory memory) address)
  (cond ((< address #x2000)
         (aref (memory-ram memory) (logand address #x7ff)))
        ((< address #x4000)
         (fetch (memory-ppu memory) address))
        ((= address #x4016)
         (fetch-strobe (memory-gamepad memory)))
        ((< address #x8000)
         0)
        (t
         (fetch (memory-mapper memory) address)))  )

(defmethod store ((memory memory) address value)
  (cond ((< address #x2000)
         (setf (aref (memory-ram memory) (logand address #x7ff)) value))
        ((< address #x4000)
         (store (memory-ppu memory) address value))
        ((= address #x4014)
         (%oam-dma memory value))
        ((= address #x4016)
         (reset-strobe (memory-gamepad memory)))
        ((< address #x8000)
         0)
        (t
         (store (memory-mapper memory) address value))))

(defun fetch-word (memory address)
  (let ((low-byte  (fetch memory address))
        (high-byte (fetch memory (1+ address))))
    (+ low-byte (ash high-byte 8))))

(defun fetch-indirect (memory address)
  (let* ((wrapped (+ (logand address #xff00)
                     (logand (1+ address) #xff)))
         (low-byte (fetch memory address))
         (high-byte (fetch memory wrapped)))
    (+ low-byte (ash high-byte 8))))

(defun fetch-range (memory start end)
  (loop for i from start upto end
        collect (fetch memory i)))
