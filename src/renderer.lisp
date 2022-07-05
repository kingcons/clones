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
                #:define-constant)
  (:export #:*framebuffer*))

(in-package :clones.renderer)

(defsection @renderer (:title "The Rendering Logic")
  (renderer class)
  (make-renderer function)
  (sync generic-function)
  (render-nametable function))

(define-constant +scanlines-per-frame+ 262
  :documentation "The number of scanlines rendered by the PPU per frame.")

(define-constant +cycles-per-scanline+ (/ 341 3)
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

(defun make-framebuffer ()
  (let ((screen-width 256)
        (screen-height 240))
    (static-vectors:make-static-vector (* screen-width screen-height 3))))

(deftype framebuffer ()
  '(simple-array octet (184320)))

(defvar *framebuffer* (make-framebuffer)
  "The primary framebuffer used by the emulator for drawing the NES output.")

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

(defgeneric sync (renderer cpu)
  (:documentation "Synchronize the renderer to the CPU and return the next scanline."))

(defmethod sync ((renderer renderer) (cpu cpu))
  (maybe-handle-dma renderer cpu)
  (with-accessors ((scanline renderer-scanline)) renderer
    (with-accessors ((cycles cpu-cycles)) cpu
      (when (> cycles +cycles-per-scanline+)
        (render-scanline renderer)
        (incf scanline)
        (callf #'mod scanline 262)
        (callf #'mod cycles 114)
        scanline))))

(defgeneric render-scanline (renderer)
  (:documentation "Render a scanline using RENDERER if rendering is enabled in PPUMASK."))

(defmethod render-scanline ((renderer renderer))
  (with-accessors ((scanline renderer-scanline)) renderer
    (cond ((< scanline 240)
           (render-visible-scanline renderer))
          ((= scanline 240)
           (post-render-scanline renderer))
          ((= scanline 241)
           (vblank-scanline renderer))
          ((= scanline 261)
           (pre-render-scanline renderer)))))

(defun render-visible-scanline (renderer)
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
            (render-sprite ppu scanline-buffer (aref sprites sprite))))))
    (dotimes (pixel-index 256)
      (let ((palette-index (aref scanline-buffer pixel-index)))
        (render-pixel *framebuffer* ppu scanline pixel-index palette-index)))
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

(defun render-nametable (ppu nt-index)
  "Render the nametable of the PPU selected by NT-INDEX.
Scroll information is not taken into account."
  (let ((*framebuffer* (make-framebuffer))
        (scanline-buffer (make-octet-vector 256))
        (return-address (clones.ppu::ppu-address ppu))
        (nt-address (* #x400 nt-index)))
    ;; TODO: The base address for a nametable should be offset by #x2000 right?
    ;; If viewing the nametable bytes yes, but the PPUADDR is set to the pattern
    ;; bytes to draw, not the nametable address. Setting low bits in PPUCTRL may
    ;; be the right approach here. Need to test with other ROMs/nametables.
    (setf (clones.ppu::ppu-address ppu) nt-address)
    (dotimes (scanline 240)
      (dotimes (tile 32)
        (render-tile ppu scanline-buffer (fetch-nt-byte ppu))
        (coarse-scroll-horizontal! ppu))
      (dotimes (pixel 256)
        (render-pixel *framebuffer* ppu scanline pixel (aref scanline-buffer pixel)))
      (fine-scroll-vertical! ppu))
    (setf (clones.ppu::ppu-address ppu) return-address)
    *framebuffer*))

(defun pixel-priority (bg-pixel sprite-pixel)
  ;; Pixel priority is based on the low-bits only!
  ;;
  ;; NOTE: We cheat on pixel priority by having the precomputed sprite pixels
  ;; set to 0 if the background priority attribute is set and they are non-zero.
  ;; This keeps us from having to track the priority of the sprites at each pixel.
  (cond ((and (zerop bg-pixel)
              (zerop sprite-pixel))
         0)
        ((and (zerop bg-pixel)
              (plusp sprite-pixel))
         sprite-pixel)
        ((and (zerop sprite-pixel)
              (plusp bg-pixel))
         bg-pixel)
        (t
         sprite-pixel)))

(defun render-sprite (ppu buffer sprite)
  "Given a SPRITE and a BUFFER for the current scanline, update the palette indexes
in the buffer corresponding to the pixels for the given sprite. Note that accounting
for pixel priority means RENDER-SPRITE may leave the buffer unmodified when the
palette index for the sprite has the background attribute set. TODO: As a 
future improvement, we should handle overlapping sprites correctly."
  ;; See: https://www.nesdev.org/wiki/PPU_sprite_priority
  (multiple-value-bind (pattern-low pattern-high) (fetch-scanline-bytes ppu sprite)
    (let ((high-bits (palette-high-bits ppu sprite))
          (x-offset (clones.ppu::sprite-x sprite)))
      (dotimes (tile-index 8)
        ;; TODO: Account for horizontal flipping
        (let* ((low-bits (palette-low-bits pattern-low pattern-high tile-index))
               (palette-index (+ 16 (dpb high-bits (byte 2 2) low-bits)))
               (buffer-index (min (+ x-offset tile-index) 255))
               (background-value (aref buffer buffer-index)))
          ;; TODO: Account for pixel priority when updating the buffer
          (setf (aref buffer buffer-index) (pixel-priority background-value palette-index)))))))

(defun render-tile (ppu buffer pattern-index)
  (multiple-value-bind (pattern-low pattern-high) (fetch-scanline-bytes ppu pattern-index)
    (let ((high-bits (palette-high-bits ppu pattern-index))
          (x-offset (* 8 (ldb (byte 5 0) (clones.ppu::ppu-address ppu)))))
      (dotimes (tile-index 8)
        (let* ((low-bits (palette-low-bits pattern-low pattern-high tile-index))
               (palette-index (dpb high-bits (byte 2 2) low-bits))
               (buffer-index (min (+ x-offset tile-index) 255)))
          (setf (aref buffer buffer-index) palette-index))))))

(defun render-pixel (framebuffer ppu y-index x-index palette-index)
  (let* ((color-index (read-palette ppu palette-index))
         (offset (* (+ (* y-index 256) x-index) 3))
         (rgb-value (aref +palette+ color-index)))
    (dotimes (i 3)
      (setf (aref framebuffer (+ offset i))
            (aref rgb-value i)))))
