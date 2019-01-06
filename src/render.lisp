(in-package :cl-user)

(defpackage :clones.render
  (:use :cl :clones.ppu)
  (:import-from :static-vectors
                :make-static-vector)
  (:import-from :alexandria
                :define-constant)
  (:import-from :clones.util
                :make-byte-vector
                :byte-vector
                :ub8)
  (:export #:*framebuffer*
           #:+color-palette+
           #:*context*
           #:context
           #:render-scanline
           #:context-scanline
           #:context-nt-buffer
           #:context-at-buffer
           #:context-scanline
           #:context-nmi-p
           #:context-frame-p
           #:context-candidates
           #:context-bg-pixels
           #:context-sprite-pixels))

(in-package :clones.render)

(defconstant +width+               256)
(defconstant +height+              240)
(defconstant +cycles-per-scanline+ 341)

(let ((rgb-vals #(#x7C #x7C #x7C  #x00 #x00 #xFC  #x00 #x00 #xBC  #x44 #x28 #xBC
                  #x94 #x00 #x84  #xA8 #x00 #x20  #xA8 #x10 #x00  #x88 #x14 #x00
                  #x50 #x30 #x00  #x00 #x78 #x00  #x00 #x68 #x00  #x00 #x58 #x00
                  #x00 #x40 #x58  #x00 #x00 #x00  #x00 #x00 #x00  #x00 #x00 #x00
                  #xBC #xBC #xBC  #x00 #x78 #xF8  #x00 #x58 #xF8  #x68 #x44 #xFC
                  #xD8 #x00 #xCC  #xE4 #x00 #x58  #xF8 #x38 #x00  #xE4 #x5C #x10
                  #xAC #x7C #x00  #x00 #xB8 #x00  #x00 #xA8 #x00  #x00 #xA8 #x44
                  #x00 #x88 #x88  #x00 #x00 #x00  #x00 #x00 #x00  #x00 #x00 #x00
                  #xF8 #xF8 #xF8  #x3C #xBC #xFC  #x68 #x88 #xFC  #x98 #x78 #xF8
                  #xF8 #x78 #xF8  #xF8 #x58 #x98  #xF8 #x78 #x58  #xFC #xA0 #x44
                  #xF8 #xB8 #x00  #xB8 #xF8 #x18  #x58 #xD8 #x54  #x58 #xF8 #x98
                  #x00 #xE8 #xD8  #x78 #x78 #x78  #x00 #x00 #x00  #x00 #x00 #x00
                  #xFC #xFC #xFC  #xA4 #xE4 #xFC  #xB8 #xB8 #xF8  #xD8 #xB8 #xF8
                  #xF8 #xB8 #xF8  #xF8 #xA4 #xC0  #xF0 #xD0 #xB0  #xFC #xE0 #xA8
                  #xF8 #xD8 #x78  #xD8 #xF8 #x78  #xB8 #xF8 #xB8  #xB8 #xF8 #xD8
                  #x00 #xFC #xFC  #xF8 #xD8 #xF8  #x00 #x00 #x00  #x00 #x00 #x00)))
  (define-constant +color-palette+ (make-array 192 :element-type 'ub8 :initial-contents rgb-vals)
    :documentation "The NTSC color palette used by the PPU." :test #'equalp))

(declaim (type (byte-vector 184320) *framebuffer*))
(defvar *framebuffer* (make-static-vector (* +width+ +height+ 3) :element-type 'ub8)
  "A Framebuffer for graphics operations with 3 bytes per pixel for RGB.")

(defstruct context
  (scanline      0                        :type (integer 0 262))
  (dma-p         nil                      :type boolean)
  (nmi-p         nil                      :type boolean)
  (frame-p       nil                      :type boolean)
  (nt-buffer     (make-byte-vector #x20)  :type (byte-vector 32))
  (at-buffer     (make-byte-vector #x08)  :type (byte-vector 08))
  (candidates    (make-array 8)           :type (simple-vector 8)))

(defvar *context* (make-context)
  "A Render Context to cache state and avoid redundant fetches.")

(defun render-scanline (ppu)
  (with-accessors ((scanline context-scanline)) *context*
    (when (zerop (mod scanline 8))
      (fill-nt-buffer ppu))
    (when (zerop (mod scanline 32))
      (fill-at-buffer ppu))))

(defmacro restoring-coarse-x (ppu &body body)
  (alexandria:with-gensyms (backup)
    `(let ((,backup (ppu-coarse-x ,ppu)))
       (unwind-protect ,@body
         (setf (ppu-coarse-x ,ppu) ,backup)))))

(defun fill-nt-buffer (ppu)
  (restoring-coarse-x ppu
    (dotimes (tile 32)
      (setf (aref (context-nt-buffer *context*) tile) (read-nametable ppu))
      (next-tile ppu 1))))

(defun fill-at-buffer (ppu)
  (restoring-coarse-x ppu
    (dotimes (quad 8)
      (setf (aref (context-at-buffer *context*) quad) (read-attribute ppu))
      (next-tile ppu 4))))
