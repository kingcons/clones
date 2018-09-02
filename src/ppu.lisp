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
                :wrap-palette-table)
  (:import-from :clones.mappers
                :mapper
                :mirroring
                :fetch
                :fetch-chr)
  (:export #:ppu
           #:make-ppu
           #:ppu-control
           #:ppu-mask
           #:ppu-status
           #:ppu-oam-address
           #:ppu-data
           #:ppu-address
           #:ppu-coarse-x
           #:ppu-coarse-y
           #:ppu-fine-x
           #:ppu-fine-y
           #:ppu-nt-index
           #:ppu-oam
           #:ppu-nametable
           #:ppu-palette-table
           #:ppu-pattern-table
           #:x-scroll-offset
           #:y-scroll-offset
           #:vram-step
           #:sprite-base-address
           #:background-base-address
           #:sprite-size
           #:vblank-p
           #:grayscale-p
           #:show-background-left-p
           #:show-sprites-left-p
           #:show-background-p
           #:show-sprites-p
           #:emphasize-red-p
           #:emphasize-green-p
           #:emphasize-blue-p
           #:fetch
           #:read-vram))

(in-package :clones.ppu)

;;; PPU Structures
;;;   PPU docs frequently refer to: Pattern Tables, Nametables, Attributes, Palette, and OAM.
;;;   Their functions and the way they make up the address space can be confusing though.
;;;
;;;   The Pattern Table is simply 8k of graphics data stored on the game cartridge.
;;;   Several mappers offer the ability to swap between different 8k banks while running.
;;;   The 8k banks are divided into two sections, one for backgrounds and one for sprites.
;;;   Each "pattern" is 16 bytes and describes a 8x8 pixel tile with two color bits per pixel.
;;;   Thus, each 4k section holds 256 patterns used by Nametables or OAM to draw the screen.
;;;
;;;   A Nametable is 960 bytes followed by a 64 byte Attribute Table. Nametable bytes are an
;;;   index (divided by 16) into the pattern table with attribute bits selecting the palette.
;;;   A single Nametable (including its Attribute Table) is 1k but the address space presents
;;;   four nametables where the PPU hardware only has enough VRAM for two.
;;;   This is accomplished by mirroring the addresses in a scheme determined by the cartridge.
;;;
;;;   The Palette is 32 bytes mirrored into the top 256 bytes of the 16k address space.
;;;   Each byte is an index into the PPU's fixed bank of 64 colors. The palette is subdivided
;;;   into 4 4-color palettes each for backgrounds and sprites. The "zero" color in each
;;;   4-color palette is treated as transparency.
;;;
;;;   OAM is 256 bytes providing 4 bytes (pattern, attribute, and scroll info) for 64 sprites.
;;;   OAM has a separate address space without mirroring and exposed through dedicated
;;;   registers for OAM address and data at 0x2003 and 0x2004 in the CPU's Memory Map.
;;; References:
;;;  * https://wiki.nesdev.com/w/index.php/PPU_memory_map
;;;  * https://wiki.nesdev.com/w/index.php/PPU_nametables
;;;  * https://wiki.nesdev.com/w/index.php/Mirroring#Nametable_Mirroring


;; PPU Structure

(defstruct ppu
  (control       0                         :type ub8)            ; 0x2000
  (mask          0                         :type ub8)            ; 0x2001
  (status        0                         :type ub8)            ; 0x2002
  (oam-address   0                         :type ub8)            ; 0x2003/0x2004
  (address       0                         :type ub16)           ; 0x2006
  (data          0                         :type ub8)            ; 0x2007
  (coarse-x      0                         :type (integer 0 31)) ; 0x2005
  (coarse-y      0                         :type (integer 0 31)) ; 0x2005
  (fine-x        0                         :type (integer 0 7))  ; 0x2005
  (fine-y        0                         :type (integer 0 7))  ; 0x2005
  (nt-index      0                         :type (integer 0 3))  ; 0x2005
  (oam           (make-byte-vector #x100)  :type (byte-vector 256))
  (nametable     (make-byte-vector #x800)  :type (byte-vector 2048))
  (palette-table (make-byte-vector #x020)  :type (byte-vector 32))
  (pattern-table nil                       :type (or null mapper)))

;; Register Bit Helpers

(macrolet ((define-control-bit (name bit-position set unset)
             `(defun ,name (ppu)
                (if (logbitp ,bit-position (ppu-control ppu))
                    ,set
                    ,unset))))
  (define-control-bit x-scroll-offset         0  256   0)
  (define-control-bit y-scroll-offset         1  240   0)
  (define-control-bit vram-step               2  032   1)
  (define-control-bit sprite-base-address     3  4096  0)
  (define-control-bit background-base-address 4  4096  0)
  (define-control-bit sprite-size             5  16    8)
  (define-control-bit vblank-p                7  t   nil))

(macrolet ((define-mask-bit (name bit-position)
             `(defun ,name (ppu)
                (logbitp ,bit-position (ppu-mask ppu)))))
  (define-mask-bit grayscale-p            0)
  (define-mask-bit show-background-left-p 1)
  (define-mask-bit show-sprites-left-p    2)
  (define-mask-bit show-background-p      3)
  (define-mask-bit show-sprites-p         4)
  (define-mask-bit emphasize-red-p        5)
  (define-mask-bit emphasize-green-p      6)
  (define-mask-bit emphasize-blue-p       7))

;; Register Behavior

(defmethod fetch ((ppu ppu) address)
  (case (logand address 7)
    (2 (read-status ppu))
    (4 (read-oam ppu))
    (7 (read-data ppu))
    (otherwise 0)))

(defun read-status (ppu)
  (ppu-status ppu))

(defun read-oam (ppu)
  (aref (ppu-oam ppu) (ppu-oam-address ppu)))

(defun read-data (ppu)
  (ppu-data ppu))

;;; PPU Memory Map

(define-constant +mirroring+
    '(:vertical   #(0 1 0 1)
      :horizontal #(0 0 1 1)
      :lower      #(0 0 0 0)
      :upper      #(1 1 1 1))
  :documentation "A map from mirroring types to arrays of nametable offsets." :test #'equalp)

(declaim (inline nt-offset))
(defun nt-offset (mirror-type address)
  "Use the MIRROR-TYPE and ADDRESS to determine the offset of the nametable to access."
  (let* ((layout    (getf +mirroring+ mirror-type))
         (nt-index  (floor (ldb (byte 12 0) address) #x400)))
    (* #x400 (aref layout nt-index))))

(defun nt-mirror (mirror-type address)
  (+ (nt-offset mirror-type address) (wrap-nametable address)))

(defun read-vram (ppu address)
  (let ((mapper (ppu-pattern-table ppu)))
    (cond ((< address #x2000)
           (fetch-chr mapper address))
          ((< address #x3f00)
           (aref (ppu-nametable ppu) (nt-mirror (mirroring mapper) address)))
          ((< address #x4000)
           (aref (ppu-palette-table ppu) (wrap-palette-table address))))))
