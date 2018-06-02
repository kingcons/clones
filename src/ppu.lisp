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
  #(#x7C #x7C #x7C  #xFC #x00 #x00  #xBC #x00 #x00  #xBC #x28 #x44
    #x84 #x00 #x94  #x20 #x00 #xA8  #x00 #x10 #xA8  #x00 #x14 #x88
    #x00 #x30 #x50  #x00 #x78 #x00  #x00 #x68 #x00  #x00 #x58 #x00
    #x58 #x40 #x00  #x00 #x00 #x00  #x00 #x00 #x00  #x00 #x00 #x00
    #xBC #xBC #xBC  #xF8 #x78 #x00  #xF8 #x58 #x00  #xFC #x44 #x68
    #xCC #x00 #xD8  #x58 #x00 #xE4  #x00 #x38 #xF8  #x10 #x5C #xE4
    #x00 #x7C #xAC  #x00 #xB8 #x00  #x00 #xA8 #x00  #x44 #xA8 #x00
    #x88 #x88 #x00  #x00 #x00 #x00  #x00 #x00 #x00  #x00 #x00 #x00
    #xF8 #xF8 #xF8  #x3C #xBC #xFC  #x68 #x88 #xFC  #x98 #x78 #xF8
    #xF8 #x78 #xF8  #x98 #x58 #xF8  #x58 #x78 #xF8  #x44 #xA0 #xFC
    #x00 #xB8 #xF8  #x18 #xF8 #xB8  #x54 #xD8 #x58  #x98 #xF8 #x58
    #xD8 #xE8 #x00  #x78 #x78 #x78  #x00 #x00 #x00  #x00 #x00 #x00
    #xFC #xFC #xFC  #xFC #xE4 #xA4  #xF8 #xB8 #xB8  #xF8 #xB8 #xD8
    #xF8 #xB8 #xF8  #xC0 #xA4 #xF8  #xB0 #xD0 #xF0  #xA8 #xE0 #xFC
    #x78 #xD8 #xF8  #x78 #xF8 #xD8  #xB8 #xF8 #xB8  #xD8 #xF8 #xB8
    #xFC #xFC #x00  #xF8 #xD8 #xF8  #x00 #x00 #x00  #x00 #x00 #x00)
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
      (format stream "Scanline: ~D   Ctrl: ~B  Mask: ~B  Status: ~B  Address: ~4,'0x"
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
    (setf (aref oam oam-address) value)
    (incf oam-address)))

(defun read-vram (ppu address)
  (cond ((< address #x2000)
         (aref (ppu-pattern-table ppu) address))
        ((< address #x3f00)
         (aref (ppu-nametable ppu) (wrap-nametable address)))
        ((< address #x4000)
         (aref (ppu-palette-table ppu) (wrap-palette-table address)))))

(defun write-vram (ppu value)
  (with-slots (address) ppu
    (cond ((< address #x2000)
           (setf (aref (ppu-pattern-table ppu) address) value))
          ((< address #x3f00)
           (setf (aref (ppu-nametable ppu) (wrap-nametable address)) value))
          ((< address #x4000)
           (setf (aref (ppu-palette-table ppu) (wrap-palette-table address)) value)))))

(defun buffered-read (ppu)
  (with-slots (address) ppu
    (let ((result (read-vram ppu address)))
      (incf (ppu-address ppu) (vram-step ppu))
      (if (< address #x3f00)
          (prog1 (ppu-read-buffer ppu)
            (setf (ppu-read-buffer ppu) result))
          result))))

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

(defun get-attribute-byte (ppu scanline tile)
  ;; Attribute tables are 64 bytes with 1 byte for each 4x4 tile area.
  ;; So skip 8 bytes ahead for every 32 scanlines and 1 byte ahead for each 4 tiles.
  (let ((scanline-offset (* 8 (floor scanline 32))))
    (read-vram ppu (+ (base-attribute-table ppu) scanline-offset (round tile 4)))))

(defun get-bg-pattern-byte (ppu pattern-index byte-position)
  ;; The pattern table is 4k and each tile is 16 bytes so multiply the pattern-index by 16.
  ;; Note that there is a high byte and low byte for each tile spaced 8 bytes apart.
  (let ((base-address (bg-pattern-address ppu))
        (byte-offset (ecase byte-position
                       (:lo 0)
                       (:hi 8))))
    (read-vram ppu (+ base-address (* pattern-index 16) byte-offset))))

;;; PPU Rendering

(defun render-pixel (x y palette-index)
  (let ((buffer-start (* (+ (* y +width+) x) 3))
        (palette-start (* palette-index 3)))
    (dotimes (i 3)
      (setf (aref *framebuffer*   (+ buffer-start i))
            (aref +color-palette+ (+ palette-start i))))))

(defun render-scanline (ppu)
  (with-slots (scanline) ppu
    (let ((backdrop-index (wrap-palette (read-vram ppu #x3f00)))
          (background-index nil)
          (sprite-index nil))
      (dotimes (tile-index 32)
        (let* ((nametable-byte  (get-nametable-byte ppu scanline tile-index))
               (attribute-byte  (get-attribute-byte ppu scanline tile-index))
               (bg-low-byte     (get-bg-pattern-byte ppu nametable-byte :lo))
               (bg-high-byte    (get-bg-pattern-byte ppu nametable-byte :hi))
               (colors nil))
          ;; combine attribute and pattern table data to get palette index
          (loop for i from 0 to 8
                for color in colors
                do (let ((x (+ (* tile-index 8) i))
                         (y scanline))
                     (render-pixel x y color))))))))

(defun sync (ppu run-to-cycle)
  (with-slots (scanline cycles result) ppu
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
      (261 (with-vblank ()
             (setf cycles (mod cycles *cycles-per-frame*)
                   scanline 0
                   vblank-status 0
                   (getf result :new-frame) t))))
    result))
