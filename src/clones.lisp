(in-package :cl-user)

(defpackage clones
  (:use :cl)
  (:import-from :clones.display
                :init-display
                :display-frame)
  (:import-from :clones.cpu
                :make-cpu
                :cpu-memory
                :cycles
                :memory
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

(defun step-frame ()
  (loop with result = nil
        until (getf result :new-frame)
        do (let ((cycle-count (single-step *nes*)))
             (with-slots (ppu) (cpu-memory *nes*)
               (setf result (sync ppu (* cycle-count 3)))
               (when (getf result :dma)
                 (dma *nes*))
               (when (getf result :nmi)
                 (nmi *nes*))))))

(defun step-frames (count)
  (dotimes (n count)
    (step-frame))
  (sb-ext:quit))

(defun play ()
  (sdl2:init :everything)
  (init-display)
  (with-slots (ppu apu) (cpu-memory *nes*)
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
            (display-frame)
            (handle-input)))))))
