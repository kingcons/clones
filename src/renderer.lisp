(mgl-pax:define-package :clones.renderer
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.ppu)
  (:import-from :serapeum
                #:octet
                #:callf
                #:~>>)
  (:import-from :alexandria
                #:define-constant))

(in-package :clones.renderer)

(defsection @renderer (:title "The Rendering Logic")
  (renderer class)
  (make-renderer function)
  (sync generic-function)
  (renderer-framebuffer generic-function))

(define-constant +scanlines-per-frame+ 262
  :documentation "The number of scanlines rendered by the PPU per frame.")

(define-constant +cycles-per-scanline+ (/ 341 3)
  :documentation "The number of PPU clock cycles in a single scanline.
  Note that 1 CPU cycle is equivalent to 3 PPU cycles.")

(define-constant +palette+
    #(#( 84  84  84)  #(  0  30 116)  #(  8  16 144)  #( 48   0 136)
      #( 68   0 100)  #( 92   0  48)  #( 84   4   0)  #( 60  24   0)
      #( 32  42   0)  #(  8  58   0)  #(  0  64   0)  #(  0  60   0)
      #(  0  50  60)  #(  0   0   0)  #(152 150 152)  #(  8  76 196)
      #( 48  50 236)  #( 92  30 228)  #(136  20 176)  #(160  20 100)
      #(152  34  32)  #(120  60   0)  #( 84  90   0)  #( 40 114   0)
      #(  8 124   0)  #(  0 118  40)  #(  0 102 120)  #(  0   0   0)
      #(236 238 236)  #( 76 154 236)  #(120 124 236)  #(176  98 236)
      #(228  84 236)  #(236  88 180)  #(236 106 100)  #(212 136  32)
      #(160 170   0)  #(116 196   0)  #( 76 208  32)  #( 56 204 108)
      #( 56 180 204)  #( 60  60  60)  #(236 238 236)  #(168 204 236)
      #(188 188 236)  #(212 178 236)  #(236 174 236)  #(236 174 212)
      #(236 180 176)  #(228 196 144)  #(204 210 120)  #(180 222 120)
      #(168 226 144)  #(152 226 180)  #(160 214 228)  #(160 162 160))
  :documentation "The Palette information used to generate NTSC video signals
decoded as RGB by the television set. We may later support a VGA palette format
to specify this. See: https://www.nesdev.org/wiki/Palette#2C02"
  :test #'equalp)

(defun make-framebuffer ()
  (let ((screen-width 256)
        (screen-height 240))
    (make-array (* screen-width screen-height 3) :element-type 'octet)))

(deftype framebuffer ()
  '(simple-array octet (184320)))

(defclass renderer ()
  ((ppu :initarg :ppu :type ppu :accessor renderer-ppu)
   (on-nmi :initarg :on-nmi :type function :accessor renderer-on-nmi)
   (on-frame :initarg :on-frame :type function :accessor renderer-on-frame)
   (scanline :initform 0 :type (integer 0 261) :accessor renderer-scanline)
   (framebuffer :initform (make-framebuffer) :type framebuffer :accessor renderer-framebuffer)))

(defun make-renderer (&key (ppu (make-ppu)) (on-nmi (constantly nil)) on-frame)
  (make-instance 'renderer :ppu ppu :on-nmi on-nmi :on-frame on-frame))

(defgeneric sync (renderer cpu)
  (:documentation "Synchronize the renderer to the current state of the CPU."))

(defmethod sync ((renderer renderer) (cpu cpu))
  (with-accessors ((scanline renderer-scanline)) renderer
    (with-accessors ((cycles cpu-cycles)) cpu
      (when (> cycles +cycles-per-scanline+)
        (render-scanline renderer)
        (incf scanline)
        (callf #'mod scanline 262)
        (callf #'mod cycles 114)))))

(defgeneric render-scanline (renderer)
  (:documentation "Render a scanline using RENDERER if rendering is enabled in PPUMASK."))

(defmethod render-scanline ((renderer renderer))
  (with-accessors ((scanline renderer-scanline)
                   (ppu renderer-ppu)) renderer
    (let ((enabled? (rendering-enabled? ppu)))
      (cond ((< scanline 240)
             (when enabled?
               (render-visible-scanline renderer)))
            ((= scanline 241)
             (set-vblank! ppu 1)
             (new-frame! renderer ppu))
            ((= scanline 261)
             (set-vblank! ppu 0)
             (when enabled?
               (prerender-scanline renderer)))))))

(defun new-frame! (renderer ppu)
  (with-accessors ((on-frame renderer-on-frame)
                   (on-nmi renderer-on-nmi)) renderer
    (when (vblank-nmi? ppu)
      (funcall on-nmi))
    (when on-frame
      (funcall on-frame renderer))))

(defun render-visible-scanline (renderer)
  "See: https://www.nesdev.org/wiki/PPU_rendering#Cycles_1-256"
  (let ((ppu (renderer-ppu renderer)))
    (dotimes (tile 32)
      (render-tile renderer ppu)
      (coarse-scroll-horizontal! ppu))
    (fine-scroll-vertical! ppu)))

(defun render-tile (renderer ppu)
  ;; Rough code for rendering an individual tile below
  (let ((nametable-byte (fetch-nt-byte ppu))
        (attribute-byte (fetch-at-byte ppu)))
    (multiple-value-bind (pattern-low pattern-high) (fetch-pattern-bytes ppu nametable-byte)
      (dotimes (pixel-index 8)
        (let* ((palette-low-bits (dpb (ldb (byte 1 (- 7 pixel-index)) pattern-high)
                                      (byte 1 1)
                                      (ldb (byte 1 (- 7 pixel-index)) pattern-low)))
               (palette-high-bits (case (quad-position ppu)
                                    (:top-left (ldb (byte 2 0) attribute-byte))
                                    (:top-right (ldb (byte 2 2) attribute-byte))
                                    (:bottom-left (ldb (byte 2 4) attribute-byte))
                                    (:bottom-right (ldb (byte 2 6) attribute-byte))))
               (palette-index (dpb palette-high-bits (byte 2 2) palette-low-bits))
               (color-index (read-palette ppu palette-index)))
          #+nil
          (format t "~2,'0B ~2,'0B ~4,'0B~%" palette-low-bits palette-high-bits palette-index)
          (render-pixel renderer color-index pixel-index))))))

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
  "See: https://www.nesdev.org/wiki/PPU_pattern_tables#Addressing"
  (let* ((address (clones.ppu::ppu-address ppu))
         (fine-y-bits (ldb (byte 3 12) address))
         (bg-table (ldb (byte 1 4) (clones.ppu::ppu-ctrl ppu)))
         (pt-index
           (~>> fine-y-bits
                (dpb 0 (byte 1 3))
                (dpb nt-byte (byte 8 4))
                (dpb bg-table (byte 1 12)))))
    (values
     (clones.mappers:get-chr (clones.ppu::ppu-pattern-table ppu) pt-index)
     (clones.mappers:get-chr (clones.ppu::ppu-pattern-table ppu) (+ pt-index 8)))))

(defun quad-position (ppu)
  (let* ((address (clones.ppu::ppu-address ppu))
         (on-right (logbitp 1 address))
         (on-bottom (logbitp 6 address)))
    (cond ((and on-right on-bottom)  :bottom-right)
          (on-bottom                 :bottom-left)
          (on-right                  :top-right)
          (t                         :top-left))))

(defun render-pixel (renderer color-index pixel-index)
  (with-accessors ((ppu renderer-ppu)
                   (scanline renderer-scanline)
                   (framebuffer renderer-framebuffer)) renderer
    (let* ((coarse-x (ldb (byte 5 0) (clones.ppu::ppu-address ppu)))
           (offset (* (+ (* scanline 256)
                         (* coarse-x 8)
                         pixel-index)
                      3))
           (rgb-value (aref +palette+ color-index)))
      (dotimes (i 3)
        (setf (aref framebuffer (+ offset i))
              (aref rgb-value i))))))

(defun coarse-scroll-horizontal! (ppu)
  "A scroll operation that conceptually occurs at the end of each 8-pixel tile."
  (symbol-macrolet ((nt-index (ldb (byte 1 10) (clones.ppu::ppu-address ppu)))
                    (coarse-x (ldb (byte 5 0) (clones.ppu::ppu-address ppu))))
    (cond ((= coarse-x 31)
           (setf coarse-x 0
                 nt-index (if (zerop nt-index) 1 0)))
          (t
           (incf coarse-x)))))

(defun fine-scroll-vertical! (ppu)
  "A scroll operation that conceptually occurs at the end of each scanline."
  (symbol-macrolet ((nt-index (ldb (byte 1 11) (clones.ppu::ppu-address ppu)))
                    (coarse-y (ldb (byte 5 5) (clones.ppu::ppu-address ppu)))
                    (fine-y (ldb (byte 3 12) (clones.ppu::ppu-address ppu))))
    (when (< fine-y 7)
      (return-from fine-scroll-vertical! (incf fine-y)))
    (setf fine-y 0)
    (cond ((= coarse-y 29)
           (setf coarse-y 0
                 nt-index (if (zerop nt-index) 1 0)))
          ((= coarse-y 31)
           (error 'not-yet-implemented))
          (t
           (incf coarse-y)))))

(defun prerender-scanline (renderer)
  (let ((ppu (renderer-ppu renderer)))
    (setf (clones.ppu::ppu-address ppu) (clones.ppu::ppu-scroll ppu))))
