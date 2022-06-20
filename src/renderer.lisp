(mgl-pax:define-package :clones.renderer
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.ppu)
  (:import-from :serapeum
                #:octet
                #:octet-vector
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

(defvar *debug-framebuffer* (make-framebuffer)
  "A framebuffer for use by debugging tools, not tied to the main renderer.")

(defclass renderer ()
  ((ppu :initarg :ppu :type ppu :accessor renderer-ppu)
   (on-nmi :initarg :on-nmi :type function :accessor renderer-on-nmi)
   (on-frame :initarg :on-frame :type function :accessor renderer-on-frame)
   (scanline :initform 0 :type (integer 0 261) :accessor renderer-scanline)
   (bg-bits :initform (make-array 256) :type octet-vector :accessor renderer-bg-bits
            :documentation "An array of palette indexes for the background of the current scanline.")
   (sprite-bits :initform (make-array 256) :type octet-vector :accessor renderer-sprite-bits
                :documentation "An array of palette indexes for the sprites on the current scanline.")))

(defun make-renderer (&key (ppu (make-ppu)) (on-nmi (constantly nil)) on-frame)
  (make-instance 'renderer :ppu ppu :on-nmi on-nmi :on-frame on-frame))

(defgeneric sync (renderer cpu)
  (:documentation "Synchronize the renderer to the CPU and return the next scanline."))

(defmethod sync ((renderer renderer) (cpu cpu))
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
                   (bg-bits renderer-bg-bits)
                   (sprite-bits renderer-sprite-bits)) renderer
    (unless (rendering-enabled? ppu)
      (return-from render-visible-scanline nil))
    (flet ((writer-callback (scanline-x-index value)
             (setf (aref bg-bits scanline-x-index) value)))
      (when (render-background? ppu)
        (dotimes (tile 32)
          (render-tile ppu #'writer-callback)
          (coarse-scroll-horizontal! ppu))))
    (when (render-sprites? ppu)
      (render-sprites renderer ppu))
    (dotimes (pixel-index 256)
      (symbol-macrolet ((bg-pixel (aref bg-bits pixel-index))
                        (sprite-pixel (aref sprite-bits pixel-index)))
        (let ((palette-index (pixel-priority bg-pixel sprite-pixel)))
          (render-pixel ppu scanline pixel-index palette-index))))
    ;; TODO: Do we need to clear the bits between scanlines?
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
  (let ((return-address (clones.ppu::ppu-address ppu))
        (nt-address (* #x400 nt-index)))
    ;; TODO: The base address for a nametable should be offset by #x2000 right?
    ;; If viewing the nametable bytes yes, but the PPUADDR is set to the pattern
    ;; bytes to draw, not the nametable address. Setting low bits in PPUCTRL may
    ;; be the right approach here. Need to test with other ROMs/nametables.
    (setf (clones.ppu::ppu-address ppu) nt-address)
    (dotimes (scanline 240)
      (flet ((writer-callback (scanline-x-index value)
               (let ((*framebuffer* *debug-framebuffer*))
                 (render-pixel ppu scanline scanline-x-index value))))
        (dotimes (tile 32)
          (render-tile ppu #'writer-callback)
          (coarse-scroll-horizontal! ppu)))
      (fine-scroll-vertical! ppu))
    (setf (clones.ppu::ppu-address ppu) return-address)
    *debug-framebuffer*))

(defun pixel-priority (bg-pixel sprite-pixel)
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

(defun render-sprites (renderer ppu)
  (let ((visible-sprites (evaluate-sprites renderer ppu))
        (sprite-bits (renderer-sprite-bits renderer)))
    (dotimes (pixel 256)
      ;; Pick first visible sprite at pixel and populate sprite-bits with its value
      ;; If the sprite-pixel is non-zero and background priority is set, supply 0.
      )))

(defun evaluate-sprites (renderer ppu)
  (let ((scanline (renderer-scanline renderer))
        (visible-sprites (make-array 8 :initial-element nil))
        (current-sprite 0))
    (dotimes (sprite-index 64)
      (let ((candidate-y (aref (clones.ppu::ppu-oam ppu) (* sprite-index 4))))
        (when (< candidate-y scanline (+ candidate-y 7))
          (if (= current-sprite 8)
              (set-sprite-overflow! ppu 1)
              (setf (aref visible-sprites current-sprite) (make-sprite ppu sprite-index)
                    current-sprite (1+ current-sprite))))))
    visible-sprites))

(defun render-tile (ppu writer-callback)
  (let* ((nametable-byte (fetch-nt-byte ppu))
         (attribute-byte (fetch-at-byte ppu))
         (coarse-x-offset (* 8 (ldb (byte 5 0) (clones.ppu::ppu-address ppu)))))
    (multiple-value-bind (pattern-low pattern-high) (fetch-pattern-bytes ppu nametable-byte)
      (let ((palette-high-bits (ecase (quad-position ppu)
                                 (:top-left (ldb (byte 2 0) attribute-byte))
                                 (:top-right (ldb (byte 2 2) attribute-byte))
                                 (:bottom-left (ldb (byte 2 4) attribute-byte))
                                 (:bottom-right (ldb (byte 2 6) attribute-byte)))))
        (dotimes (pixel-index 8)
          (let* ((palette-low-bits (dpb (ldb (byte 1 (- 7 pixel-index)) pattern-high)
                                        (byte 1 1)
                                        (ldb (byte 1 (- 7 pixel-index)) pattern-low)))
                 (palette-index (dpb palette-high-bits (byte 2 2) palette-low-bits)))
            (funcall writer-callback (+ coarse-x-offset pixel-index) palette-index)))))))

(defun render-pixel (ppu scanline pixel-index palette-index)
  (let* ((color-index (read-palette ppu palette-index))
         (offset (* (+ (* scanline 256)
                       pixel-index)
                    3))
         (rgb-value (aref +palette+ color-index)))
    (dotimes (i 3)
      (setf (aref *framebuffer* (+ offset i))
            (aref rgb-value i)))))

;;; extract to sprites.lisp later

(defclass sprite ()
  ((sprite-x :initarg :sprite-x :type octet :reader sprite-x)
   (sprite-y :initarg :sprite-y :type octet :reader sprite-y)
   (pattern-index :initarg :pattern-index :type octet :reader sprite-index)
   (attributes :initarg :attributes :type octet :reader sprite-attributes)))

(defun make-sprite (ppu sprite-index)
  (let ((bytes (subseq (clones.ppu::ppu-oam ppu) sprite-index)))
    (make-instance 'sprite :sprite-x (aref bytes 3)
                           :sprite-y (aref bytes 0)
                           :pattern-index (aref bytes 1)
                           :attributes (aref bytes 2))))
