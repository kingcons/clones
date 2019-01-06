(in-package :cl-user)

(defpackage clones
  (:use :cl)
  (:import-from :clones.cpu
                :make-cpu
                :cpu-memory
                :cpu-cycles
                :reset)
  (:import-from :clones.render
                :*context*
                :context-frame-p
                :context-nmi-p
                :sync)
  (:import-from :clones.input
                :handle-input)
  (:import-from :clones.instructions
                :single-step)
  (:import-from :clones.memory
                :memory-ppu
                :memory-gamepad
                :memory-dma-p
                :swap-rom)
  (:import-from :clones.util
                :slot->))

(in-package :clones)

(defconstant +cycles-per-scanline+ 113)
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

(defun step-frame (nes &optional (headless nil))
  (with-accessors ((ppu memory-ppu)
                   (gamepad memory-gamepad)) (cpu-memory nes)
    (loop
      (let* ((cycle-count (single-step nes)))
        (when *trace*
          (print nes)
          (clones.disassembler:now nes))
        (sync ppu (* cycle-count 3))
        (when (memory-dma-p (cpu-memory nes))
          (clones.cpu:dma nes))
        (when (context-nmi-p *context*)
          (clones.cpu:nmi nes))
        (when (context-frame-p *context*)
          (when headless (return nil))
          (let ((input (handle-input gamepad)))
            (when (eq :quit input) (return nil))
            (clones.display:display-frame)))))))

(defun step-frames (frame-count)
  (dotimes (i frame-count)
    (step-frame *nes* t)))

(defun play ()
  (sdl2:init :everything)
  (clones.display:init-display)
  (loop for result = (step-frame *nes*)
        until (null result))
  (sdl2:quit))
