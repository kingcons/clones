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
                :wrap-byte
                :wrap-nametable
                :wrap-palette-table)
  (:import-from :clones.mappers
                :mapper
                :mirroring
                :fetch
                :fetch-chr
                :store
                :store-chr
                :default-rom)
  (:export #:ppu
           #:make-ppu
           #:set-vblank
           #:ppu-coarse-x
           #:ppu-coarse-y
           #:ppu-fine-x
           #:ppu-fine-y
           #:ppu-nt-index
           #:show-background-p
           #:show-sprites-p
           #:fetch
           #:store
           #:next-tile
           #:next-line
           #:read-nametable
           #:read-attribute
           #:read-pattern
           #:quad-position
           #:palette-high-bits
           #:palette-low-bits))

(in-package :clones.ppu)

;;; PPU Data Structures
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
;;;   Thus, a Nametable can specify a full background of 32x30 tiles (each being 8x8 pixels).
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
  (write-latch   0                         :type bit)
  (oam           (make-byte-vector #x100)  :type (byte-vector 256))
  (nametable     (make-byte-vector #x800)  :type (byte-vector 2048))
  (palette-table (make-byte-vector #x020)  :type (byte-vector 32))
  (pattern-table (default-rom)             :type (or null mapper)))

;; Register Bit Helpers

(macrolet ((define-control-bit (name bit-position set unset)
             `(defun ,name (ppu)
                (if (logbitp ,bit-position (ppu-control ppu))
                    ,set
                    ,unset))))
  (define-control-bit x-scroll-offset   0  256   0)
  (define-control-bit y-scroll-offset   1  240   0)
  (define-control-bit vram-step         2  032   1)
  (define-control-bit sprite-offset     3  4096  0)
  (define-control-bit background-offset 4  4096  0)
  (define-control-bit sprite-size       5  16    8)
  (define-control-bit vblank-p          7  t   nil))

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

(defun set-vblank (ppu state)
  (setf (ldb (byte 1 7) (ppu-status ppu)) state))

;; Register Behavior

(defmethod fetch ((ppu ppu) address)
  (case (logand address 7)
    (2 (read-status ppu))
    (4 (read-oam ppu))
    (7 (read-data ppu))
    (otherwise 0)))

(defun read-status (ppu)
  (with-accessors ((status ppu-status)) ppu
    (setf (ppu-write-latch ppu) 0)
    (prog1 status
      (setf status (logand status #x7f)))))

(defun read-oam (ppu)
  (aref (ppu-oam ppu) (ppu-oam-address ppu)))

(defun read-data (ppu)
  (with-accessors ((address ppu-address) (data ppu-data)) ppu
    (let ((new-value (read-vram ppu address)))
      (incf address (vram-step ppu))
      (if (< address #x3f00)
          (prog1 data
            (setf data new-value))
          new-value))))

(defmethod store ((ppu ppu) address value)
  (case (logand address 7)
    (0 (write-control ppu value))
    (1 (setf (ppu-mask ppu) value))
    (2 0)
    (3 (setf (ppu-oam-address ppu) value))
    (4 (write-oam ppu value))
    (5 (write-scroll ppu value))
    (6 (write-address ppu value))
    (7 (write-vram ppu value))))

(defun write-control (ppu value)
  (setf (ppu-control ppu) value
        (ppu-nt-index ppu) (logand value 3)))

(defun write-oam (ppu value)
  (with-accessors ((oam-address ppu-oam-address) (oam ppu-oam)) ppu
    (setf (aref oam oam-address) value
          oam-address (wrap-byte (1+ oam-address)))))

(defun write-scroll (ppu value)
  (with-accessors ((write-latch ppu-write-latch)
                   (coarse-x    ppu-coarse-x)
                   (coarse-y    ppu-coarse-y)
                   (fine-x      ppu-fine-x)
                   (fine-y      ppu-fine-y)) ppu
    (if (zerop write-latch)
        (setf write-latch 1
              coarse-x (ldb (byte 5 3) value)
              fine-x   (ldb (byte 3 0) value))
        (setf write-latch 0
              coarse-y (ldb (byte 5 3) value)
              fine-y   (ldb (byte 3 0) value)))))

;; KLUDGE: Technically, writes to PPUADDR clobber matching values in PPUSCROLL.
;; However, as written in https://wiki.nesdev.com/w/index.php/PPU_scrolling#The_common_case
;; it is not only customary but expected that PPUSCROLL will be set after PPUADDR.
;; Thus, it should be safe to skip modifying the scroll register in almost all circumstances.
(let ((buffer 0))
  (defun write-address (ppu value)
    (with-accessors ((write-latch ppu-write-latch)
                     (address     ppu-address)) ppu
      (if (zerop write-latch)
          (setf write-latch 1
                buffer (ash value 8))
          (setf write-latch 0
                address (logior buffer value))))))

;;; PPU Memory Map

(define-constant +mirroring+
    '(:vertical   #(0 1 0 1)
      :horizontal #(0 0 1 1)
      :lower      #(0 0 0 0)
      :upper      #(1 1 1 1))
  :documentation "A map from mirroring types to arrays of nametable offsets." :test #'equalp)

(defun nt-offset (mirror-type nt-index)
  "Use the MIRROR-TYPE and ADDRESS to determine the offset of the nametable to access."
  (let ((layout (getf +mirroring+ mirror-type)))
    (* #x400 (aref layout nt-index))))

(defun nt-mirror (mirror-type address)
  (let ((nt-index (floor (ldb (byte 12 0) address) #x400)))
    (+ (nt-offset mirror-type nt-index) (wrap-nametable address))))

(defun read-vram (ppu address)
  (let ((mapper (ppu-pattern-table ppu)))
    (cond ((< address #x2000)
           (fetch-chr mapper address))
          ((< address #x3f00)
           (aref (ppu-nametable ppu) (nt-mirror (mirroring mapper) address)))
          ((< address #x4000)
           (aref (ppu-palette-table ppu) (wrap-palette-table address))))))

(defun write-vram (ppu value)
  (with-accessors ((address ppu-address)) ppu
    (let ((mapper (ppu-pattern-table ppu)))
      (cond ((< address #x2000)
             (store-chr mapper address value))
            ((< address #x3f00)
             (setf (aref (ppu-nametable ppu) (nt-mirror (mirroring mapper) address)) value))
            ((< address #x4000)
             (setf (aref (ppu-palette-table ppu) (wrap-palette-table address)) value))))
    (incf address (vram-step ppu))))

;; PPU Internal Fetches

(defun next-tile (ppu step)
  (with-accessors ((coarse-x ppu-coarse-x)
                   (nt-index ppu-nt-index)) ppu
    (let ((new-value (+ coarse-x step)))
      (if (< new-value 32)
          (setf coarse-x new-value)
          (setf nt-index (logxor nt-index 1)
                coarse-x (logand new-value 31))))))

(defun next-line (ppu)
  (with-accessors ((fine-y   ppu-fine-y)
                   (coarse-y ppu-coarse-y)
                   (nt-index ppu-nt-index)) ppu
    (when (= fine-y 7)
      (if (< coarse-y 29)
          (incf coarse-y)
          (setf coarse-y 0
                nt-index (logxor nt-index 2))))
    (setf fine-y (logand (1+ fine-y) 7))))

(defun nametable-address (ppu)
  (let ((mirror-type (mirroring (ppu-pattern-table ppu))))
    (+ #x2000 (nt-offset mirror-type (ppu-nt-index ppu)))))

(defun read-nametable (ppu)
  (let* ((nametable-offset (nametable-address ppu))
         (vertical-offset (* (ppu-coarse-y ppu) 32))
         (address (+ nametable-offset vertical-offset (ppu-coarse-x ppu))))
    (read-vram ppu address)))

(defun read-attribute (ppu)
  (let* ((nametable-offset (+ #x3c0 (nametable-address ppu)))
         (vertical-offset (* (ppu-coarse-y ppu) 8))
         (quad (floor (ppu-coarse-x ppu) 4))
         (address (+ nametable-offset vertical-offset quad)))
    (read-vram ppu address)))

(defun read-pattern (ppu pattern-bank-offset tile-number line-offset byte)
  ;; TODO: Shouldn't LINE-OFFSET almost always be PPU-FINE-Y for both bgs and sprites?
  (let* ((tile-offset (* tile-number 16))
         (byte-offset (ecase byte
                        (:lo 0)
                        (:hi 8)))
         (address (+ pattern-bank-offset tile-offset line-offset byte-offset)))
    (read-vram ppu address)))

(defun quad-position (coarse-x coarse-y)
  (if (evenp (floor coarse-y 2))
      (if (evenp (floor coarse-x 2)) 0 2)
      (if (evenp (floor coarse-x 2)) 4 6)))

(defun palette-high-bits (attribute-byte coarse-x coarse-y)
  (let ((position (quad-position coarse-x coarse-y)))
    (ldb (byte 2 position) attribute-byte)))

(defun palette-low-bits (pattern-lo pattern-hi row)
  (+ (ash (ldb (byte 1 row) pattern-hi) 1)
     (ldb (byte 1 row) pattern-lo)))
