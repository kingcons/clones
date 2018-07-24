(in-package :cl-user)

(defpackage clones
  (:use :cl)
  (:import-from :clones.cpu
                :make-cpu
                :cpu-memory
                :cycles
                :reset
                :nmi
                :dma)
  (:import-from :clones.ppu
                :*cycles-per-frame*
                :sync)
  (:import-from :clones.input
                :handle-input)
  (:import-from :clones.instructions
                :single-step)
  (:import-from :clones.memory
                :ppu
                :gamepad
                :swap-rom)
  (:import-from :clones.util
                :slot->))

(in-package :clones)

(defvar *nes* (make-cpu))
(defvar *trace* nil)

(defun change-game (rom-file)
  ;; TODO: Reset other CPU+PPU slots, RAM, etc as necessary.
  (swap-rom (cpu-memory *nes*) rom-file)
  (reset *nes*))

(defun quit ()
  (format t "Thanks for playing!~%~%")
  #+sbcl (sb-ext:quit)
  #+ccl (ccl:quit))

(defun play (&optional (frame-count -1))
  (sdl2:init :everything)
  (clones.display:init-display)
  (with-slots (ppu gamepad) (cpu-memory *nes*)
    (loop
      (let ((cycle-count (single-step *nes*)))
        (when *trace*
          (print *nes*)
          (clones.disassembler:now *nes*))
        (let ((ppu-result (sync ppu (* cycle-count 3))))
          (when (getf ppu-result :dma)
            (dma *nes*))
          (when (getf ppu-result :nmi)
            (nmi *nes*))
          (when (getf ppu-result :new-frame)
            (clones.display:display-frame)
            (let ((input (handle-input gamepad)))
              (when (eq :quit input)
                (return nil))
              (when (= clones.display:*frame-count* frame-count)
                (return nil))))))))
  (sdl2:quit))
