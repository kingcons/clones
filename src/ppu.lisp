(in-package :cl-user)

(defpackage :clones.ppu
  (:use :cl)
  (:import-from :alexandria
                :define-constant)
  (:import-from :static-vectors
                :make-static-vector)
  (:import-from :clones.mappers
                :mapper
                :load-chr
                :store-chr)
  (:import-from :clones.util
                :ub8
                :ub16
                :byte-vector
                :make-byte-vector
                :wrap-byte
                :wrap-nametable
                :wrap-palette-table
                :wrap-palette)
  (:export #:*framebuffer*
           #:*cycles-per-frame*
           #:ppu
           #:ppu-result
           #:ppu-cycles
           #:ppu-cartridge
           #:make-ppu
           #:ppu-read
           #:ppu-write
           #:sync))

(in-package :clones.ppu)

(defvar *cycles-per-scanline* 341)
(defvar *cycles-per-frame*  89342)

(defconstant +width+ 256)
(defconstant +height+ 240)

;;; Core PPU Data Structures

(declaim (type (byte-vector 184320) *framebuffer*))
(defvar *framebuffer* (make-static-vector (* +width+ +height+ 3) :element-type 'ub8)
  "A Framebuffer for graphics operations with 3 bytes per pixel for RGB.")

(define-constant +color-palette+
  #(#x7C #x7C #x7C  #x00 #x00 #xFC  #x00 #x00 #xBC  #x44 #x28 #xBC
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
    #x00 #xFC #xFC  #xF8 #xD8 #xF8  #x00 #x00 #x00  #x00 #x00 #x00)
  :documentation "The color palette used by the graphics card." :test #'equalp)

(defstruct context
  (nt-buffer     (make-byte-vector #x20) :type (byte-vector 32))
  (at-buffer     (make-byte-vector #x08) :type (byte-vector 08))
  (bg-buffer     (make-byte-vector #x08) :type (byte-vector 08))
  (sprite-buffer (make-byte-vector #x08) :type (byte-vector 08)))

(defvar *render-context* (make-context))

(defstruct ppu
  (dma-result    nil                       :type boolean)
  (nmi-result    nil                       :type boolean)
  (new-frame     nil                       :type boolean)
  (cycles        0                         :type fixnum)
  (scanline      0                         :type (integer 0 262))
  (read-buffer   0                         :type ub8)
  (control       0                         :type ub8)  ; 0x2000
  (mask          0                         :type ub8)  ; 0x2001
  (status        0                         :type ub8)  ; 0x2002
  (oam-address   0                         :type ub8)  ; 0x2003
  (scroll-x      0                         :type ub8)  ; 0x2005
  (scroll-y      0                         :type ub8)  ; 0x2005
  (address       0                         :type ub16) ; 0x2006
  (scroll-dir    :x                        :type keyword)
  (address-byte  :high                     :type keyword)
  (nt-buffer     (make-byte-vector #x20)   :type (byte-vector 32))
  (at-buffer     (make-byte-vector #x08)   :type (byte-vector 08))
  (oam           (make-byte-vector #x100)  :type (byte-vector 256))
  (nametable     (make-byte-vector #x800)  :type (byte-vector 2048))
  (palette-table (make-byte-vector #x020)  :type (byte-vector 32))
  (cartridge     nil                       :type (or null mapper)))

(defmethod print-object ((ppu ppu) stream)
  (print-unreadable-object (ppu stream :type t)
    (with-slots (scanline control mask status address) ppu
      (format stream "Scanline:~3d  Ctrl:~8,'0b  Mask:~8,'0b  Status:~8,'0b  Address:~4,'0x"
              scanline control mask status address))))

;;; PPU Register Helpers

(defmacro define-ppu-bit (name (&rest args) &body body)
  `(progn
     (defun ,name ,(append '(ppu) args)
       ,@body)))

(defmacro defcontrol (name bit-position then else)
  `(define-ppu-bit ,name ()
       (if (zerop (logand (ppu-control ppu) ,(expt 2 bit-position)))
           ,then
           ,else)))

(defcontrol x-scroll-offset         0  0  +width+)
(defcontrol y-scroll-offset         1  0  +height+)
(defcontrol vram-step               2  1  #x20)
(defcontrol sprite-pattern-address  3  0  #x1000)
(defcontrol bg-pattern-address      4  0  #x1000)
(defcontrol sprite-size             5  8  16)
(defcontrol vblank-nmi              7 nil t)

(defmacro defmask (name bit-position)
  `(define-ppu-bit ,name ()
     (not (zerop (logand (ppu-mask ppu) ,(expt 2 bit-position))))))

(defmask grayscale          0)
(defmask show-bg-left       1)
(defmask show-sprites-left  2)
(defmask show-bg            3)
(defmask show-sprites       4)
(defmask strong-reds        5)
(defmask strong-greens      6)
(defmask strong-blues       7)

(defmacro defstatus (name bit)
  `(define-ppu-bit ,name (value)
     (setf (ldb (byte 1 ,bit) (ppu-status ppu)) value)))

(defstatus set-sprite-overflow 5)
(defstatus set-sprite-zero-hit 6)

(defmacro with-vblank (() &body body)
  `(symbol-macrolet ((vblank-status (ldb (byte 1 7) (ppu-status ppu)))
                     (vblank-nmi    (ldb (byte 1 7) (ppu-control ppu))))
     ,@body))

;;; PPU Memory Map

(defun read-status (ppu)
  (setf (ppu-scroll-dir ppu) :x
        (ppu-address-byte ppu) :high)
  (ppu-status ppu))

(defun ppu-read (ppu address)
  (case (logand address 7)
    (2 (read-status ppu))
    (4 (read-oam ppu))
    (7 (buffered-read ppu))
    (otherwise 0)))

;; TODO: Handle scroll offsets properly.
(defun ppu-write (ppu address value)
  (case (logand address 7)
    (0 (setf (ppu-control ppu) value))
    (1 (setf (ppu-mask ppu) value))
    (3 (setf (ppu-oam-address ppu) value))
    (4 (write-oam ppu value))
    (5 (update-scroll ppu value))
    (6 (update-address ppu value))
    (7 (write-vram ppu value))
    (otherwise 0)))

(defun read-oam (ppu)
  (with-accessors ((oam ppu-oam) (oam-address ppu-oam-address)) ppu
    (aref oam oam-address)))

(defun write-oam (ppu value)
  (with-accessors ((oam ppu-oam) (oam-address ppu-oam-address)) ppu
    (setf (aref oam oam-address) value
          oam-address (wrap-byte (1+ oam-address)))))

(defun read-vram (ppu address)
  (declare (optimize speed)
           (type ub16 address))
  (cond ((< address #x2000)
         (load-chr (ppu-cartridge ppu) address))
        ((< address #x3f00)
         (aref (ppu-nametable ppu) (wrap-nametable address)))
        ((< address #x4000)
         (aref (ppu-palette-table ppu) (palette-index address)))))

(defun write-vram (ppu value)
  (with-accessors ((address ppu-address)) ppu
    (cond ((< address #x2000)
           (store-chr (ppu-cartridge ppu) address value))
          ((< address #x3f00)
           (setf (aref (ppu-nametable ppu) (wrap-nametable address)) value))
          ((< address #x4000)
           (setf (aref (ppu-palette-table ppu) (palette-index address)) value)))
    (incf address (vram-step ppu))))

(defun buffered-read (ppu)
  (with-accessors ((address ppu-address)) ppu
    (let ((result (read-vram ppu address)))
      (incf address (vram-step ppu))
      (if (< address #x3f00)
          (prog1 (ppu-read-buffer ppu)
            (setf (ppu-read-buffer ppu) result))
          result))))

(defun palette-index (address)
  (let ((result (if (and (> address #x3f0f)
                         (zerop (mod address 4)))
                    (- address 16)
                    address)))
    (wrap-palette-table result)))

;; TODO: Handle scroll offsets properly.
(defun update-scroll (ppu value)
  (with-accessors ((scroll-x ppu-scroll-x)
                   (scroll-y ppu-scroll-y)
                   (scroll-dir ppu-scroll-dir)) ppu
    (case scroll-dir
      (:x (setf scroll-x value
                scroll-dir :y))
      (:y (setf scroll-y value
                scroll-dir :x)))))

;; TODO: Handle scroll offsets properly.
(defun update-address (ppu value)
  (with-accessors ((address ppu-address) (address-byte ppu-address-byte)) ppu
    (case address-byte
      (:high (setf address (logior (logand address #xff) (ash value 8))
                   address-byte :low))
      (:low (setf address (logior (logand address #xff00) value)
                  address-byte :high)))))

;;; Rendering Helpers

(defun color-quadrant (scanline tile)
  ;; Each attribute byte holds the two high bits of the palette index for 4 16x16 pixel areas.
  ;; The two bit pairs are laid out in the byte from bit 0 to 8 like so: tl . tr . bl . br
  ;; Where tl is top-left, tr is top-right, and so on. Since left/right and top/bottom
  ;; alternate every 16 pixels, we can use the even divisibility of scanlines/16 and tiles/2.
  (let ((vertical (if (evenp (floor scanline 16))
                      :top
                      :bottom))
        (horizontal (if (evenp (floor tile 2))
                        :left
                        :right)))
    (if (eq vertical :top)
        (if (eq horizontal :left) 0 2)
        (if (eq horizontal :left) 4 6))))

(defun base-nametable (ppu)
  (case (ldb (byte 2 0) (ppu-control ppu))
    (0 #x2000)
    (1 #x2400)
    (2 #x2800)
    (3 #x2c00)))

(defun base-attribute-table (ppu)
  ;; Attribute tables always start #x3c0 bytes into a nametable.
  (+ (base-nametable ppu) #x3c0))

(defun get-nametable-byte (ppu scanline tile)
  ;; A nametable of 8x8 tiles for a 256x240 screen is laid out 32x30.
  ;; So skip 32 bytes ahead for every 8 scanlines and 1 byte ahead for each tile.
  (let* ((scanline-offset (* 32 (floor scanline 8)))
         (address (+ (base-nametable ppu) scanline-offset tile)))
    (read-vram ppu address)))

(defun get-attribute-byte (ppu scanline quad)
  ;; Attribute table is 64 bytes with 1 byte for each 4x4 tile area (quad).
  ;; So skip 8 bytes ahead for every 32 scanlines and 1 byte ahead per quad.
  (let* ((scanline-offset (* 8 (floor scanline 32)))
         (base-address (base-attribute-table ppu))
         (address (+ base-address scanline-offset quad)))
    (read-vram ppu address)))

(defun get-bg-pattern-byte (ppu scanline pattern-index byte-position)
  ;; The pattern table is 4k and each tile is 16 bytes. Multiply the pattern index
  ;; from the nametable by 16 to get a starting offset. Then, each horizontal row
  ;; in the tile is represented by 1 byte, so use (scanline % 8) to get the final offset.
  ;; Note that there is a high byte and low byte for each tile spaced 8 bytes apart.
  (let* ((base-address (bg-pattern-address ppu))
         (tile-offset (* pattern-index 16))
         (line-offset (mod scanline 8))
         (byte-offset (ecase byte-position
                        (:lo 0)
                        (:hi 8)))
         (address (+ base-address tile-offset line-offset byte-offset)))
    (read-vram ppu address)))

(defun get-palette-index (attribute-bits pattern-bits)
  (declare (optimize speed)
           (type ub8 attribute-bits pattern-bits))
  ;; The attribute byte determines the two high-bits of the palette index.
  ;; The pattern-table low-byte and high-byte determine the 0th and 1st bit.
  (dpb attribute-bits (byte 2 2) pattern-bits))

(defun get-palette-index-high (scanline tile attribute-byte)
  (declare (optimize speed)
           (type ub8 attribute-byte))
  ;; Retrieve the 2 high bits of the palette index from the attribute byte.
  (let ((bit-position (color-quadrant scanline tile)))
    (ldb (byte 2 bit-position) attribute-byte)))

(defun get-palette-index-low (pattern-low-byte pattern-high-byte bit)
  (declare (optimize speed)
           (type ub8 pattern-low-byte pattern-high-byte))
  ;; Retrieve the 0th and 1st bit of the palette index from the pattern bytes.
  (+ (ldb (byte 1 bit) pattern-low-byte)
     (ash (ldb (byte 1 bit) pattern-high-byte) 1)))

(defun get-color (ppu type index)
  ;; Retrieve a color from the palette table based on the pixel type and index.
  (let ((base-address (ecase type
                        (:bg     #x3f00)
                        (:sprite #x3f10))))
     (read-vram ppu (+ base-address index))))

;;; PPU Rendering

(defun render-pixel (x y palette-index)
  (declare (optimize speed)
           (type ub8 x y palette-index))
  (let ((buffer-start (* (+ (* y +width+) x) 3))
        (palette-start (* palette-index 3)))
    (dotimes (i 3)
      (setf (aref *framebuffer*   (+ buffer-start i))
            (aref +color-palette+ (+ palette-start i))))))

(defun compute-bg-colors (ppu scanline tile)
  (declare (optimize speed))
  (with-accessors ((at-buffer context-at-buffer)
                   (nt-buffer context-nt-buffer)
                   (bg-buffer context-bg-buffer)) *render-context*
    (let* ((nt-byte   (aref nt-buffer tile))
           (at-byte   (aref at-buffer (floor tile 4)))
           (low-byte  (get-bg-pattern-byte ppu scanline nt-byte :lo))
           (high-byte (get-bg-pattern-byte ppu scanline nt-byte :hi))
           (palette-high-bits (get-palette-index-high scanline tile at-byte)))
      (loop for bit from 0 to 7
            for palette-low-bits = (get-palette-index-low low-byte high-byte bit)
            for index = (get-palette-index palette-high-bits palette-low-bits)
            do (setf (aref bg-buffer bit) (if (zerop (logand index #x3))
                                              0
                                              (get-color ppu :bg index)))))))

(defun fill-name-table-buffer (ppu scanline)
  (with-accessors ((nt-buffer context-nt-buffer)) *render-context*
    (dotimes (tile 32)
      (setf (aref nt-buffer tile) (get-nametable-byte ppu scanline tile)))))

(defun fill-attribute-table-buffer (ppu scanline)
  (with-accessors ((at-buffer context-at-buffer)) *render-context*
    (dotimes (quad 8)
      (setf (aref at-buffer quad) (get-attribute-byte ppu scanline quad)))))

(defun prefill-sprite-buffer (ppu scanline)
  (with-accessors ((sprite-buffer context-sprite-buffer)) *render-context*
    (let ((oam (ppu-oam ppu))
          (size (sprite-size ppu))
          (count 0))
      (dotimes (i (length sprite-buffer))
        (setf (aref sprite-buffer i) 0))
      (dotimes (i 64)
        ;; We bump Y here because we're considering sprites for the _next_ scanline.
        (let ((sprite-y (1+ (aref oam (* i 4)))))
          (declare (type ub8 sprite-y scanline size))
          (when (< sprite-y scanline (+ sprite-y size))
            (if (= count 8)
                (return (set-sprite-overflow ppu 1))
                (setf (aref sprite-buffer count) i
                      count (1+ count)))))))))

(defun render-tile (ppu scanline tile)
  (with-accessors ((bg-buffer context-bg-buffer)
                   (sprite-buffer context-sprite-buffer)) *render-context*
    (let ((backdrop-color (wrap-palette (read-vram ppu #x3f00))))
      (compute-bg-colors ppu scanline tile)
      (loop for i from 7 downto 0
            for bg-color across bg-buffer
            do (let ((x (+ (* tile 8) i)))
                 (if (zerop bg-color)
                     (render-pixel x scanline backdrop-color)
                     (render-pixel x scanline bg-color)))))))

(defun render-scanline (ppu)
  (let ((scanline (ppu-scanline ppu)))
    (when (zerop (mod scanline 8))
      (fill-name-table-buffer ppu scanline))
    (when (zerop (mod scanline 32))
      (fill-attribute-table-buffer ppu scanline))
    (dotimes (tile 32)
      (render-tile ppu scanline tile))
    (prefill-sprite-buffer ppu scanline)))

(defun sync (ppu run-to-cycle)
  (with-accessors ((scanline ppu-scanline)
                   (cycles ppu-cycles)
                   (nmi-result ppu-nmi-result)
                   (new-frame ppu-new-frame)) ppu
    (setf nmi-result nil new-frame nil)
    (when (< run-to-cycle (+ cycles *cycles-per-scanline*))
      (return-from sync))
    (when (< scanline +height+)
      (render-scanline ppu))
    (incf scanline)
    (incf cycles *cycles-per-scanline*)
    (case scanline
      (241 (with-vblank ()
             (setf vblank-status 1)
             (when (plusp vblank-nmi)
               (setf nmi-result t))))
      (262 (with-vblank ()
             (setf scanline 0
                   vblank-status 0
                   new-frame t))))))
