(in-package :cl-user)

(defpackage :clones.ppu
  (:use :cl)
  (:import-from :alexandria
                :define-constant)
  (:import-from :static-vectors
                :make-static-vector)
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
           #:make-ppu
           #:ppu-read
           #:ppu-write
           #:initialize-pattern-table
           #:sync))

(in-package :clones.ppu)

(defvar *cycles-per-scanline* 341)
(defvar *cycles-per-frame*  89342)

(defconstant +width+ 256)
(defconstant +height+ 240)

;;; Core PPU Data Structures

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

(defstruct ppu
  (result        '(:new-frame nil :nmi nil :dma nil) :type cons)
  (cycles        0                                   :type fixnum)
  (scanline      0                                   :type fixnum)
  (read-buffer   0                                   :type ub8)
  (control       0                                   :type ub8)  ; 0x2000
  (mask          0                                   :type ub8)  ; 0x2001
  (status        0                                   :type ub8)  ; 0x2002
  (oam-address   0                                   :type ub8)  ; 0x2003
  (scroll-x      0                                   :type ub8)  ; 0x2005
  (scroll-y      0                                   :type ub8)  ; 0x2005
  (address       0                                   :type ub16) ; 0x2006
  (scroll-dir    :x                                  :type keyword)
  (address-byte  :high                               :type keyword)
  (oam           (make-byte-vector #x100)            :type byte-vector)
  (nametable     (make-byte-vector #x800)            :type byte-vector)
  (palette-table (make-byte-vector #x020)            :type byte-vector)
  (pattern-table (make-byte-vector #x2000)           :type byte-vector))

(defmethod print-object ((ppu ppu) stream)
  (print-unreadable-object (ppu stream :type t)
    (with-slots (scanline control mask status address) ppu
      (format stream "Scanline:~3d  Ctrl:~8,'0b  Mask:~8,'0b  Status:~8,'0b  Address:~4,'0x"
              scanline control mask status address))))

;;; PPU Register Helpers

(defmacro define-ppu-bit (name (&rest args) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,(append '(ppu) args)
       (declare (type ppu ppu))
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

;; KLUDGE: Just copy the CHR into PPU at boot time until we figure out bank switching.
(defun initialize-pattern-table (ppu rom)
  (setf (ppu-pattern-table ppu) (clones.rom::rom-chr rom)))

(declaim (inline read-status))
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
  (with-slots (oam oam-address) ppu
    (aref oam oam-address)))

(defun write-oam (ppu value)
  (with-slots (oam oam-address) ppu
    (setf (aref oam oam-address) value
          oam-address (wrap-byte (1+ oam-address)))))

(defun read-vram (ppu address)
  (cond ((< address #x2000)
         (aref (ppu-pattern-table ppu) address))
        ((< address #x3f00)
         (aref (ppu-nametable ppu) (wrap-nametable address)))
        ((< address #x4000)
         (aref (ppu-palette-table ppu) (palette-index address)))))

(defun write-vram (ppu value)
  (with-slots (address) ppu
    (cond ((< address #x2000)
           (setf (aref (ppu-pattern-table ppu) address) value))
          ((< address #x3f00)
           (setf (aref (ppu-nametable ppu) (wrap-nametable address)) value))
          ((< address #x4000)
           (setf (aref (ppu-palette-table ppu) (palette-index address)) value)))
    (incf address (vram-step ppu))))

(defun buffered-read (ppu)
  (with-slots (address) ppu
    (let ((result (read-vram ppu address)))
      (incf (ppu-address ppu) (vram-step ppu))
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
  (with-slots (scroll-x scroll-y scroll-dir) ppu
    (case scroll-dir
      (:x (setf scroll-x value
                scroll-dir :y))
      (:y (setf scroll-y value
                scroll-dir :x)))))

;; TODO: Handle scroll offsets properly.
(defun update-address (ppu value)
  (with-slots (address address-byte) ppu
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

(declaim (inline base-attribute-table))
(defun base-attribute-table (ppu)
  ;; Attribute tables always start #x3c0 bytes into a nametable.
  (+ (base-nametable ppu) #x3c0))

(defun get-nametable-byte (ppu scanline tile)
  ;; A nametable of 8x8 tiles for a 256x240 screen is laid out 32x30.
  ;; So skip 32 bytes ahead for every 8 scanlines and 1 byte ahead for each tile.
  (let ((scanline-offset (* 32 (floor scanline 8))))
    (read-vram ppu (+ (base-nametable ppu) scanline-offset tile))))

(defun get-attribute-bits (ppu scanline tile)
  ;; Attribute tables are 64 bytes with 1 byte for each 4x4 tile area.
  ;; So skip 8 bytes ahead for every 32 scanlines and 1 byte ahead for each 4 tiles.
  ;; However, only 2 bits of the attribute table are relevant per tile.
  (let* ((scanline-offset (* 8 (floor scanline 32)))
         (tile-offset (round tile 4))
         (base-address (base-attribute-table ppu))
         (attribute-byte (read-vram ppu (+ base-address scanline-offset tile-offset)))
         (byte-position (color-quadrant scanline tile)))
    (ldb (byte 2 byte-position) attribute-byte)))

(defun get-bg-pattern-byte (ppu pattern-index byte-position)
  ;; The pattern table is 4k and each tile is 16 bytes so multiply the pattern-index by 16.
  ;; Note that there is a high byte and low byte for each tile spaced 8 bytes apart.
  (let ((base-address (bg-pattern-address ppu))
        (byte-offset (ecase byte-position
                       (:lo 0)
                       (:hi 8))))
    (read-vram ppu (+ base-address (* pattern-index 16) byte-offset))))

(defun get-palette-index (index-high-bits low-byte high-byte bit-position)
  ;; The attribute table byte determines the two high-bits of the four bit palette index.
  ;; The pattern-table low-byte and high-byte determine the 0th and 1st bit in the index.
  (let ((index-low-bits (+ (ldb (byte 1 bit-position) low-byte)
                           (ash (ldb (byte 1 bit-position) high-byte) 1))))
    (dpb index-high-bits (byte 2 2) index-low-bits)))

(defun get-color (ppu type index)
  (let ((base-address (ecase type
                        (:bg     #x3f00)
                        (:sprite #x3f10))))
     (read-vram ppu (+ base-address index))))

;;; PPU Rendering

(defun render-pixel (x y palette-index)
  (let ((buffer-start (* (+ (* y +width+) x) 3))
        (palette-start (* palette-index 3)))
    (dotimes (i 3)
      (setf (aref *framebuffer*   (+ buffer-start i))
            (aref +color-palette+ (+ palette-start i))))))

(defun compute-bg-colors (ppu nametable-byte attribute-bits)
  (let ((low-byte  (get-bg-pattern-byte ppu nametable-byte :lo))
        (high-byte (get-bg-pattern-byte ppu nametable-byte :hi)))
    (loop for bit from 0 to 7
          for index = (get-palette-index attribute-bits low-byte high-byte bit)
          collect (get-color ppu :bg index))))

(defun render-scanline (ppu)
  (with-slots (scanline) ppu
    (let ((backdrop-color (wrap-palette (read-vram ppu #x3f00))))
      (dotimes (tile-index 32)
        (let* ((nametable-byte  (get-nametable-byte ppu scanline tile-index))
               (attribute-bits  (get-attribute-bits ppu scanline tile-index))
               (bg-colors (compute-bg-colors ppu nametable-byte attribute-bits)))
          (loop for i from 0 to 7
                for color in bg-colors
                do (let ((x (+ (* tile-index 8) i)))
                     (render-pixel x scanline color))))))))

(defun sync (ppu run-to-cycle)
  (with-slots (scanline cycles result) ppu
    (setf (getf result :nmi) nil
          (getf result :new-frame) nil)
    (when (< run-to-cycle (+ cycles *cycles-per-scanline*))
      (return-from sync result))
    (when (< scanline +height+)
      (render-scanline ppu))
    (incf scanline)
    (incf cycles *cycles-per-scanline*)
    (case scanline
      (241 (with-vblank ()
             (setf vblank-status 1)
             (when (plusp vblank-nmi)
               (setf (getf result :nmi) t))))
      (262 (with-vblank ()
             (setf scanline 0
                   vblank-status 0
                   (getf result :new-frame) t))))
    result))
