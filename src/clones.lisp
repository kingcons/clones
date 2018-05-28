(in-package :cl-user)

(defpackage clones
  (:use :cl)
  (:import-from :clones.display
                :init-display
                :display-frame)
  (:import-from :clones.cpu
                :make-cpu
                :cpu-memory
                :reset)
  (:import-from :clones.instructions
                :scanline-step)
  (:import-from :clones.memory
                :swap-rom))

(in-package :clones)

(defvar *nes* (make-cpu))
(defvar *trace* nil)

(defun change-game (rom-file)
  (swap-rom (cpu-memory *nes*) rom-file)
  (reset *nes*))

(defun tracing ()
  (when *trace*
    (format t *nes*)
    (clones.disassembler:now)))

(defun play ()
  ;; KLUDGE: SCANLINE-STEP should probably be a generic function if we're using it like this.
  (init-display)
  (with-slots (ppu apu) (cpu-memory *nes*)
    (loop
      (scanline-step *nes*)
      (tracing)
      (let ((ppu-result (clones.ppu:scanline-step ppu)))
        (when (getf ppu-result :vblank-nmi)
          (nmi *nes*))
        (when (getf ppu-result :new-frame)
          (display-frame))))))
