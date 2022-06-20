(mgl-pax:define-package :clones.memory
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.mappers)
  (:import-from :clones.ppu
                #:ppu
                #:make-ppu
                #:read-ppu
                #:write-ppu)
  (:import-from :clones.input
                #:controller
                #:make-controller
                #:read-controller
                #:reset-controller)
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
  (fetch-indirect function)
  (get-ppu (reader memory))
  (get-cart (reader memory))
  (get-controller (reader memory))
  (swap-cart function))

(defclass memory ()
  ((ram :initarg :ram :type octet-vector)
   (ppu :initarg :ppu :type ppu :reader get-ppu)
   (cart :initarg :cart :type mapper :reader get-cart)
   (controller :initarg :controller :type controller :reader get-controller)))

(defun make-memory (&key (ram (make-octet-vector #x800)) (ppu (make-ppu))
                      (controller (make-controller)) (cart (load-rom)))
  (make-instance 'memory :ram ram :ppu ppu :controller controller :cart cart))

(defun swap-cart (memory relative-path)
  (let* ((path (asdf:system-relative-pathname :clones relative-path))
         (rom (clones.mappers:load-rom path)))
    (with-slots (ppu cart) memory
      (setf (slot-value ppu 'clones.ppu::pattern-table) rom
            cart rom))))

(defun fetch (memory address)
  (with-slots (ram ppu cart controller) memory
    (cond ((< address #x2000) ;; read from RAM
           (aref ram (logand address #x7ff)))
          ((< address #x4000) ;; read from PPU
           (read-ppu ppu (logand address #x7)))
          ((= address #x4016)
           (read-controller controller))
          ((< address #x8000) ;; read from peripherals (ppu, apu, input)
           0)
          (t ;; read from cartridge
           (get-prg cart address)))))

(defun store (memory address value)
  (with-slots (ram ppu cart controller) memory
    (cond ((< address #x2000) ;; write to RAM
           (setf (aref ram (logand address #x7ff)) value))
          ((< address #x4000) ;; write to PPU
           (write-ppu ppu (logand address #x7) value))
          ((= address #x4016)
           (reset-controller controller))
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
