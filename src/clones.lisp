(mgl-pax:define-package :clones
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.renderer)
  (:import-from :clones.ppu
                #:ppu
                #:make-ppu))

(in-package :clones)

(defun change-game (cpu relative-path)
  (let* ((path (asdf:system-relative-pathname :clones relative-path))
         (rom (clones.mappers:load-rom path))
         (memory (clones.cpu:cpu-memory cpu))
         (ppu (slot-value memory 'clones.memory::ppu)))
    (setf (slot-value memory 'clones.memory::cart) rom
          (slot-value ppu 'clones.ppu::pattern-table) rom)))

(defun main (cpu renderer &optional rom)
  (when rom
    (change-game cpu rom))
  (clones.cpu:reset cpu)
  (loop for count = (single-step cpu)
        do (progn
             ;; (now cpu)
             (sync renderer cpu))))
