(mgl-pax:define-package :clones.renderer
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.ppu))

(in-package :clones.renderer)

(defsection @renderer (:title "The Rendering Logic")
  (make-renderer function))

(defclass renderer ()
  ((ppu :initarg :ppu :type ppu)
   (scanline :initform 0 :type (integer 0 262))
   (on-nmi :initarg :on-nmi :type function)))

(defun make-renderer (ppu on-nmi)
  (make-instance 'renderer :ppu ppu :on-nmi on-nmi))

(defun combine-tile-bytes (low-byte high-byte)
  "Given the low and high byte of a pattern table tile,
   construct the appropriate bitfield for its pixels.
   See: https://www.nesdev.org/wiki/PPU_pattern_tables"
  (flet ((interleave-bits (i)
           (+ (ash (ldb (byte 1 i) high-byte) 1)
              (ldb (byte 1 i) low-byte))))
    (loop with result = 0
          for i below 8
          do (setf result (dpb
                           (interleave-bits i)
                           (byte 2 (* i 2))
                           result))
          finally (return result))))
