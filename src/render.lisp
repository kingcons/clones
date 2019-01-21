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
           #:+width+
           #:+height+
           #:*context*
           #:context
           #:sync
           #:context-scanline
           #:context-nt-buffer
           #:context-at-buffer
           #:context-nmi-p
           #:context-frame-p
           #:context-candidates))

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
  (cycles        0                        :type fixnum)
  (scanline      0                        :type (integer 0 262))
  (dma-p         nil                      :type boolean)
  (nmi-p         nil                      :type boolean)
  (frame-p       nil                      :type boolean)
  (nt-buffer     (make-byte-vector #x20)  :type (byte-vector 32))
  (at-buffer     (make-byte-vector #x08)  :type (byte-vector 08))
  (candidates    (make-array 8)           :type (simple-vector 8)))

(defvar *context* (make-context)
  "A Render Context to cache state and avoid redundant fetches.")

(defun sync (ppu cycle-count)
  (with-accessors ((cycles context-cycles)
                   (scanline context-scanline)) *context*
    (when (< cycle-count (+ cycles +cycles-per-scanline+))
      (return-from sync))
    (when (< scanline +height+)
      (render-scanline ppu scanline))
    (incf cycles +cycles-per-scanline+)
    (incf scanline)
    (case scanline
      (241 (start-vblank ppu))
      (262 (finish-frame ppu)))))

(defun render-scanline (ppu scanline)
  (prepare-context ppu scanline)
  (let ((bg-pixels (make-byte-vector 8))
        (sprite-pixels (make-byte-vector 8)))
    (declare (dynamic-extent bg-pixels sprite-pixels))
    (dotimes (tile 32)
      (render-tile ppu tile bg-pixels sprite-pixels)))
  (next-line ppu))

(defun prepare-context (ppu scanline)
  (with-accessors ((frame-p context-frame-p)
                   (nmi-p   context-nmi-p)) *context*
    (setf frame-p nil nmi-p nil)
    (when (zerop (mod scanline 8))
      (fill-nt-buffer ppu))
    (when (zerop (mod scanline 32))
      (fill-at-buffer ppu))))

(defun start-vblank (ppu)
  (set-vblank ppu 1)
  (when (vblank-p ppu)
    (setf (context-nmi-p *context*) t)))

(defun finish-frame (ppu)
  (set-vblank ppu 0)
  (setf (context-frame-p *context*) t
        (context-scanline *context*) 0))

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

(defun render-tile (ppu tile bg-pixels sprite-pixels)
  (with-accessors ((scanline   context-scanline)
                   (nt-buffer  context-nt-buffer)
                   (at-buffer  context-at-buffer)
                   (candidates context-candidates)) *context*
    (let* ((nametable-byte (aref nt-buffer tile))
           (attribute-byte (aref at-buffer (floor tile 4))))
      (get-bg-pixels ppu nametable-byte attribute-byte bg-pixels)
      (loop for i from 7 downto 0
            for tile-x = (+ (* tile 8) i)
            for color-index across bg-pixels
            do (render-pixel scanline tile-x color-index)))))

(defun render-pixel (scanline tile-x color-index)
  (let ((framebuffer-offset (* (+ (* scanline +width+) tile-x) 3))
        (palette-offset (* color-index 3)))
    (dotimes (i 3)
      (setf (aref *framebuffer* (+ framebuffer-offset i))
            (aref +color-palette+ (+ palette-offset i))))))
