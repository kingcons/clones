(mgl-pax:define-package :clones.renderer
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.ppu)
  (:import-from :clones.cpu
                #:cpu
                #:cpu-cycles
                #:cpu-memory)
  (:import-from :clones.memory
                #:memory-dma?)
  (:import-from :serapeum
                #:octet
                #:octet-vector
                #:make-octet-vector
                #:callf)
  (:import-from :alexandria
                #:define-constant))

(in-package :clones.renderer)

(defsection @renderer (:title "The Rendering Logic")
  (renderer class)
  (make-renderer function)
  (sync generic-function))

(define-constant +scanlines-per-frame+ 262
  :documentation "The number of scanlines rendered by the PPU per frame.")

(define-constant +cycles-per-scanline+ (round 341 3)
  :documentation "The number of PPU clock cycles in a single scanline.
  Note that 1 CPU cycle is equivalent to 3 PPU cycles.")

(define-constant +palette+
    #(#(#x7C #x7C #x7C)  #(#x00 #x00 #xFC)  #(#x00 #x00 #xBC)  #(#x44 #x28 #xBC)
      #(#x94 #x00 #x84)  #(#xA8 #x00 #x20)  #(#xA8 #x10 #x00)  #(#x88 #x14 #x00)
      #(#x50 #x30 #x00)  #(#x00 #x78 #x00)  #(#x00 #x68 #x00)  #(#x00 #x58 #x00)
      #(#x00 #x40 #x58)  #(#x00 #x00 #x00)  #(#x00 #x00 #x00)  #(#x00 #x00 #x00)
      #(#xBC #xBC #xBC)  #(#x00 #x78 #xF8)  #(#x00 #x58 #xF8)  #(#x68 #x44 #xFC)
      #(#xD8 #x00 #xCC)  #(#xE4 #x00 #x58)  #(#xF8 #x38 #x00)  #(#xE4 #x5C #x10)
      #(#xAC #x7C #x00)  #(#x00 #xB8 #x00)  #(#x00 #xA8 #x00)  #(#x00 #xA8 #x44)
      #(#x00 #x88 #x88)  #(#x00 #x00 #x00)  #(#x00 #x00 #x00)  #(#x00 #x00 #x00)
      #(#xF8 #xF8 #xF8)  #(#x3C #xBC #xFC)  #(#x68 #x88 #xFC)  #(#x98 #x78 #xF8)
      #(#xF8 #x78 #xF8)  #(#xF8 #x58 #x98)  #(#xF8 #x78 #x58)  #(#xFC #xA0 #x44)
      #(#xF8 #xB8 #x00)  #(#xB8 #xF8 #x18)  #(#x58 #xD8 #x54)  #(#x58 #xF8 #x98)
      #(#x00 #xE8 #xD8)  #(#x78 #x78 #x78)  #(#x00 #x00 #x00)  #(#x00 #x00 #x00)
      #(#xFC #xFC #xFC)  #(#xA4 #xE4 #xFC)  #(#xB8 #xB8 #xF8)  #(#xD8 #xB8 #xF8)
      #(#xF8 #xB8 #xF8)  #(#xF8 #xA4 #xC0)  #(#xF0 #xD0 #xB0)  #(#xFC #xE0 #xA8)
      #(#xF8 #xD8 #x78)  #(#xD8 #xF8 #x78)  #(#xB8 #xF8 #xB8)  #(#xB8 #xF8 #xD8)
      #(#x00 #xFC #xFC)  #(#xF8 #xD8 #xF8)  #(#x00 #x00 #x00)  #(#x00 #x00 #x00))
  :documentation "The Palette information used to generate NTSC video signals
decoded as RGB by the television set. We may later support a VGA palette format
to specify this. See: https://www.nesdev.org/wiki/Palette#2C02"
  :test #'equalp)

(defclass renderer ()
  ((ppu :initarg :ppu :type ppu :accessor renderer-ppu)
   (on-nmi :initarg :on-nmi :type function :accessor renderer-on-nmi)
   (on-frame :initarg :on-frame :type function :accessor renderer-on-frame)
   (scanline :initform 0 :type (integer 0 261) :accessor renderer-scanline)
   (scanline-buffer :initform (make-octet-vector 256) :type octet-vector :accessor renderer-scanline-buffer
                    :documentation "A vector of palette indexes for the current scanline.")))

(defun make-renderer (&key (ppu (make-ppu)) (on-nmi (constantly nil)) on-frame)
  (make-instance 'renderer :ppu ppu :on-nmi on-nmi :on-frame on-frame))

(defun maybe-handle-dma (renderer cpu)
  (when (memory-dma? (cpu-memory cpu))
    (incf (cpu-cycles cpu) 61)
    (incf (renderer-scanline renderer) 4)
    (setf (memory-dma? (cpu-memory cpu)) nil)))

(defgeneric sync (renderer cpu framebuffer)
  (:documentation "Synchronize the renderer to the CPU and return the next scanline."))

(defmethod sync ((renderer renderer) (cpu cpu) framebuffer)
  (maybe-handle-dma renderer cpu)
  (with-accessors ((scanline renderer-scanline)) renderer
    (with-accessors ((cycles cpu-cycles)) cpu
      (when (> cycles +cycles-per-scanline+)
        (render-scanline renderer framebuffer)
        (incf scanline)
        (callf #'mod scanline +scanlines-per-frame+)
        (callf #'mod cycles +cycles-per-scanline+)
        scanline))))

(defgeneric render-scanline (renderer framebuffer)
  (:documentation "Render a scanline into FRAMEBUFFER using RENDERER when rendering is enabled."))

(defmethod render-scanline ((renderer renderer) framebuffer)
  (with-accessors ((scanline renderer-scanline)) renderer
    (cond ((< scanline 240)
           (render-visible-scanline renderer framebuffer))
          ((= scanline 240)
           (post-render-scanline renderer))
          ((= scanline 241)
           (vblank-scanline renderer))
          ((= scanline 261)
           (pre-render-scanline renderer)))))

(defun render-visible-scanline (renderer framebuffer)
  "See: https://www.nesdev.org/wiki/PPU_rendering#Cycles_1-256"
  (with-accessors ((ppu renderer-ppu)
                   (scanline renderer-scanline)
                   (scanline-buffer renderer-scanline-buffer)) renderer
    (unless (rendering-enabled? ppu)
      (return-from render-visible-scanline nil))
    (when (render-background? ppu)
      (dotimes (tile 32)
        (render-tile ppu scanline-buffer (fetch-nt-byte ppu))
        (coarse-scroll-horizontal! ppu)))
    (when (render-sprites? ppu)
      (let ((sprites (evaluate-sprites ppu scanline)))
        (dotimes (sprite 8)
          (when (aref sprites sprite)
            (render-tile ppu scanline-buffer (aref sprites sprite))))))
    (dotimes (pixel-index 256)
      (let ((palette-index (aref scanline-buffer pixel-index)))
        (render-pixel framebuffer ppu scanline pixel-index palette-index)))
    (fine-scroll-vertical! ppu)))

(defun post-render-scanline (renderer)
  (with-accessors ((on-frame renderer-on-frame)) renderer
    (funcall on-frame renderer)))

(defun vblank-scanline (renderer)
  (with-accessors ((ppu renderer-ppu)) renderer
    (set-vblank! ppu 1)
    (when (vblank-nmi? ppu)
      (funcall (renderer-on-nmi renderer)))))

(defun pre-render-scanline (renderer)
  (with-accessors ((ppu renderer-ppu)) renderer
    (set-vblank! ppu 0)
    (when (rendering-enabled? ppu)
      (setf (clones.ppu::ppu-address ppu) (clones.ppu::ppu-scroll ppu))
      (set-sprite-overflow! ppu 0))))

(defun pixel-priority (bg-index sp-index)
  ;; NOTE: We cheat on pixel priority by having the precomputed sprite pixels
  ;; set to 0 if the background priority attribute is set and they are non-zero.
  (let ((bg-bits (ldb (byte 2 0) bg-index))
        (sp-bits (ldb (byte 2 0) sp-index))
        (sprite-index (+ 16 sp-index)))
    (cond ((and (zerop bg-bits)
                (zerop sp-bits))
           0)
          ((and (zerop bg-bits)
                (plusp sp-bits))
           sprite-index)
          ((and (zerop sp-bits)
                (plusp bg-bits))
           bg-index)
          (t
           sprite-index))))

(defun render-tile (ppu buffer tile)
  ;; TODO: Likely doesn't handle overlapping sprites correctly. Needs investigation.
  ;;  Specifically, we iterate through the sprites in order rather than "back to front".
  ;; See: https://www.nesdev.org/wiki/PPU_sprite_priority
  (multiple-value-bind (low-byte high-byte) (fetch-scanline-bytes ppu tile)
    (let ((x-offset (compute-x-offset ppu tile))
          (high-bits (palette-high-bits ppu tile))
          (flipped? (flip-x? tile)))
      (dotimes (x-index 8)
        (let* ((tile-index (if flipped? (- 7 x-index) x-index))
               (low-bits (palette-low-bits low-byte high-byte tile-index))
               (palette-index (dpb high-bits (byte 2 2) low-bits))
               (buffer-index (min (+ x-offset x-index) 255))
               (previous-value (aref buffer buffer-index)))
          (setf (aref buffer buffer-index)
                (etypecase tile
                  (sprite (pixel-priority previous-value palette-index))
                  (fixnum palette-index))))))))

(defun render-pixel (framebuffer ppu y-index x-index palette-index)
  (let* ((color-index (read-palette ppu palette-index))
         (offset (* (+ (* y-index 256) x-index) 3))
         (rgb-value (aref +palette+ color-index)))
    (dotimes (i 3)
      (setf (aref framebuffer (+ offset i))
            (aref rgb-value i)))))

