(mgl-pax:define-package :clones.renderer
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.ppu)
  (:import-from :serapeum #:octet)
  (:import-from :alexandria #:define-constant))

(in-package :clones.renderer)

(defsection @renderer (:title "The Rendering Logic")
  (renderer class)
  (make-renderer function))

(define-constant +scanlines-per-frame+ 262
  :documentation "The number of scanlines rendered by the PPU per frame.")

(define-constant +cycles-per-scanline+ (/ 341 3)
  :documentation "The number of PPU clock cycles in a single scanline.
  Note that 1 CPU cycle is equivalent to 3 PPU cycles.")

(defun make-framebuffer ()
  (let ((screen-width 256)
        (screen-height 240))
    (make-array (* screen-width screen-height 3) :element-type 'octet)))

(deftype framebuffer ()
  '(simple-array (unsigned-byte 8) (184320)))

(defclass renderer ()
  ((ppu :initarg :ppu :type ppu :accessor renderer-ppu)
   (on-nmi :initarg :on-nmi :type function :accessor renderer-on-nmi)
   (scanline :initform 0 :type (integer 0 261) :accessor renderer-scanline)
   (framebuffer :initform (make-framebuffer) :type framebuffer :accessor renderer-framebuffer)))

(defun make-renderer (ppu on-nmi)
  (make-instance 'renderer :ppu ppu :on-nmi on-nmi))

(defgeneric sync (renderer cpu)
  (:documentation "Synchronize the renderer to the current state of the CPU."))

(defmethod sync ((renderer renderer) (cpu cpu))
  (with-accessors ((scanline renderer-scanline)) renderer
    (with-accessors ((cycles cpu-cycles)) cpu
      (when (> cycles +cycles-per-scanline+)
        (render-scanline renderer)
        (setf scanline (mod (1+ scanline) 262))
        (setf cycles (mod cycles 114))))))

(defgeneric render-scanline (renderer)
  (:documentation "Render a scanline using RENDERER if rendering is enabled in PPUMASK."))

(defmethod render-scanline ((renderer renderer))
  (with-accessors ((scanline renderer-scanline)
                   (on-nmi renderer-on-nmi)
                   (ppu renderer-ppu)) renderer
    (cond ((< scanline 240)
           (render-visible-scanline renderer))
          ((= scanline 241)
           (set-vblank! ppu 1)
           (funcall on-nmi))
          ((= scanline 261)
           (set-vblank! ppu 0)
           (prerender-scanline renderer)))))

(defun render-visible-scanline (renderer)
  ;; If scanline 0-239, do some drawing to the framebuffer.
  )

(defun prerender-scanline (renderer)
  ;; If scanline is 261, reload vertical scroll bits if rendering is enabled.
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
