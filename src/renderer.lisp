(mgl-pax:define-package :clones.renderer
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.ppu)
  (:import-from :serapeum
                #:octet
                #:~>>)
  (:import-from :alexandria
                #:define-constant))

(in-package :clones.renderer)

(defsection @renderer (:title "The Rendering Logic")
  (renderer class)
  (make-renderer function)
  (sync generic-function))

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
  '(simple-array octet (184320)))

(defclass renderer ()
  ((ppu :initarg :ppu :type ppu :accessor renderer-ppu)
   (on-nmi :initarg :on-nmi :type function :accessor renderer-on-nmi)
   (scanline :initform 0 :type (integer 0 261) :accessor renderer-scanline)
   (framebuffer :initform (make-framebuffer) :type framebuffer :accessor renderer-framebuffer)))

(defun make-renderer (&key (ppu (make-ppu)) (on-nmi (constantly nil)))
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
           (when (vblank-nmi? ppu)
             (funcall on-nmi)))
          ((= scanline 261)
           (set-vblank! ppu 0)
           (prerender-scanline renderer)))))

(defun render-visible-scanline (renderer)
  ;; As long as either background rendering or sprite rendering is enabled, draw to the framebuffer.
  (let ((ppu (renderer-ppu renderer)))
    (when (rendering-enabled? ppu)
      (let ((nametable-byte (fetch-nt-byte ppu))
            (attribute-byte (fetch-at-byte ppu)))
        (multiple-value-bind (pattern-lo pattern-hi) (fetch-pattern-bytes ppu nametable-byte)
          (break))))))

(defun fetch-nt-byte (ppu)
  "See: https://www.nesdev.org/wiki/PPU_scrolling#Tile_and_attribute_fetching"
  (let ((nt-index (ldb (byte 12 0) (clones.ppu::ppu-address ppu))))
    (aref (clones.ppu::ppu-name-table ppu) nt-index)))

(defun fetch-at-byte (ppu)
  "See: https://www.nesdev.org/wiki/PPU_scrolling#Tile_and_attribute_fetching"
  (let* ((address (clones.ppu::ppu-address ppu))
         (coarse-x-bits (ldb (byte 3 2) address))
         (coarse-y-bits (ldb (byte 3 7) address))
         (nt-select (ldb (byte 2 10) address))
         (attribute-offset #b1111)
         (at-index
           (~>> coarse-x-bits
                (dpb coarse-y-bits (byte 3 3))
                (dpb attribute-offset (byte 4 6))
                (dpb nt-select (byte 2 10)))))
    (aref (clones.ppu::ppu-name-table ppu) at-index)))

(defun fetch-pattern-bytes (ppu nt-byte)
  (let* ((address (clones.ppu::ppu-address ppu))
         (fine-y-bits (ldb (byte 3 12) address))
         (bg-table (ldb (byte 1 4) (clones.ppu::ppu-ctrl ppu)))
         (pt-index
           (~>> fine-y-bits
                (dpb 0 (byte 1 3))
                (dpb nt-byte (byte 8 4))
                (dpb bg-table (byte 1 12)))))
    (values
     (aref (clones.ppu::ppu-pattern-table ppu) pt-index)
     (aref (clones.ppu::ppu-pattern-table ppu) (+ pt-index 8)))))

(defun coarse-scroll-horizontal! (ppu)
  "A scroll operation that conceptually occurs at the end of each 8-pixel tile."
  (let ((address (clones.ppu::ppu-address ppu)))
    (symbol-macrolet ((nt-index (ldb (byte 1 10) address))
                      (coarse-x (ldb (byte 5 0) address)))
      (cond ((= coarse-x 31)
             (setf coarse-x 0
                   nt-index (if (zerop nt-index) 1 0)))
            (t
             (incf coarse-x))))))

(defun fine-scroll-vertical! (ppu)
  "A scroll operation that conceptually occurs at the end of each scanline."
  (let ((address (clones.ppu::ppu-address ppu)))
    (symbol-macrolet ((nt-index (ldb (byte 1 11) address))
                      (coarse-y (ldb (byte 5 5) address))
                      (fine-y (ldb (byte 3 12) address)))
      (when (< fine-y 7)
        (return-from fine-scroll-vertical! (incf fine-y)))
      (setf fine-y 0)
      (cond ((= coarse-y 29)
             (setf coarse-y 0
                   nt-index (if (zerop nt-index) 1 0)))
            ((= coarse-y 31)
             (error 'not-yet-implemented))
            (t
             (incf coarse-y))))))

(defun prerender-scanline (renderer)
  (let ((ppu (renderer-ppu renderer)))
    (when (rendering-enabled? ppu)
      (setf (clones.ppu::ppu-address ppu) (clones.ppu::ppu-scroll ppu)))))

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
