(in-package :cl-user)

(defpackage :clones.ppu
  (:use :cl)
  (:import-from :alexandria
                :define-constant)
  (:import-from :clones.util
                :ub8
                :ub16
                :byte-vector
                :make-byte-vector
                :wrap-nametable
                :wrap-palette)
  (:export #:*cpu-cycles-per-scanline*
           #:*cpu-cycles-per-frame*
           #:ppu
           #:make-ppu
           #:ppu-read
           #:ppu-write
           #:initialize-pattern-table))

(in-package :clones.ppu)

(defvar *cpu-cycles-per-scanline* 114)
(defvar *cpu-cycles-per-frame*  29781)

;;; Core PPU Data Structures

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
  (cycles        0     :type fixnum)
  (scanline      0     :type fixnum)
  (read-buffer   0     :type ub8)
  (control       0     :type ub8)  ; 0x2000
  (mask          0     :type ub8)  ; 0x2001
  (status        0     :type ub8)  ; 0x2002
  (oam-address   0     :type ub8)  ; 0x2003
  (scroll-x      0     :type ub8)  ; 0x2005
  (scroll-y      0     :type ub8)  ; 0x2005
  (address       0     :type ub16) ; 0x2006
  (scroll-dir    :x    :type keyword)
  (address-byte  :high :type keyword)
  (oam           (make-byte-vector #x100)  :type byte-vector)
  (nametable     (make-byte-vector #x800)  :type byte-vector)
  (palette-table (make-byte-vector #x020)  :type byte-vector)
  (pattern-table (make-byte-vector #x2000) :type byte-vector))

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

(defcontrol x-scroll-offset      0  0  256)
(defcontrol y-scroll-offset      1  0  240)
(defcontrol vram-step            2  1  #x20)
(defcontrol sprite-pattern-addr  3  0  #x1000)
(defcontrol bg-pattern-addr      4  0  #x1000)
(defcontrol sprite-size          5  8  #x10)
(defcontrol vblank-nmi           7 nil t)

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
(defstatus set-in-vblank       7)

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
         (aref (ppu-palette-table ppu) (wrap-palette address)))))

(defun write-vram (ppu value)
  (with-slots (address) ppu
    (cond ((< address #x2000)
           (setf (aref (ppu-pattern-table ppu) address) value))
          ((< address #x3f00)
           (setf (aref (ppu-nametable ppu) (wrap-nametable address)) value))
          ((< address #x4000)
           (setf (aref (ppu-palette-table ppu) (wrap-palette address)) value)))))

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

;;; PPU Rendering Loop

