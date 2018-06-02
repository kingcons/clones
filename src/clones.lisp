(in-package :cl-user)

(defpackage clones
  (:use :cl)
  (:import-from :clones.display
                :init-display
                :display-frame)
  (:import-from :clones.cpu
                :make-cpu
                :cpu-memory
                :reset
                :dma)
  (:import-from :clones.ppu
                :ppu-cycles
                :*cycles-per-frame*
                :sync)
  (:import-from :clones.instructions
                :single-step)
  (:import-from :clones.memory
                :memory-ppu
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

(defun play ()
  (init-display)
  (with-slots (ppu apu) (cpu-memory *nes*)
    (loop
      (let ((cycle-count (single-step *nes*)))
        (when *trace*
          (format t *nes*)
          (clones.disassembler:now))
        (let ((ppu-result (sync ppu (* cycle-count 3))))
          (when (getf ppu-result :dma)
            (dma *nes*))
          (when (getf ppu-result :nmi)
            (nmi *nes*))
          (when (getf ppu-result :new-frame)
            (display-frame)
            (with-slots (cycles) *nes*
              (setf cycles (mod cycles (round *cycles-per-frame* 3)))
              (when *trace*
                (format t "CPU Cycles: ~5d  PPU Cycles: ~5d~%"
                        cycles (slot-> *nes* memory ppu cycles))))))))))
