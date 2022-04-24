(mgl-pax:define-package :clones.render
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.ppu))

(in-package :clones.render)

(defsection @render (:title "The Rendering Logic")
  )

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
