(mgl-pax:define-package :clones.ppu
  (:use :cl :alexandria :mgl-pax)
  (:import-from :clones.util
                #:define-printer
                #:wrap-byte)
  (:import-from :clones.mappers
                #:mapper
                #:load-rom
                #:get-chr
                #:set-chr)
  (:import-from :serapeum
                #:~>>
                #:octet
                #:octet-vector
                #:make-octet-vector))

(in-package :clones.ppu)

(defsection @ppu (:title "Picture Processing Unit")
  ;; PPU state
  (ppu class)
  (make-ppu function)
  (read-ppu function)
  (write-ppu function)
  (set-vblank! function)
  (vblank-nmi? function)
  (read-palette function)
  (get-mirroring function)
  (quad-position function)
  (render-sprites? function)
  (render-background? function)
  (rendering-enabled? function)
  ;; Sprite handling
  (sprite class)
  (make-sprite function)
  (evaluate-sprites function)
  (set-sprite-overflow! function)
  (set-sprite-zero-hit! function)
  ;; Graphics Fetching
  (fetch-nt-byte function)
  (fetch-at-byte function)
  (fetch-scanline-bytes function)
  (fetch-tile-bytes function)
  (palette-low-bits function)
  (palette-high-bits function)
  ;; Scrolling
  (fine-scroll-vertical! function)
  (coarse-scroll-horizontal! function))

(defclass ppu ()
  ((ctrl :initform 0 :type octet :accessor ppu-ctrl)
   (mask :initform 0 :type octet :accessor ppu-mask)
   (status :initform 0 :type octet :accessor ppu-status)
   (oam-addr :initform 0 :type octet :accessor ppu-oam-addr)
   (scroll :initform 0 :type (unsigned-byte 16) :accessor ppu-scroll)
   (address :initform 0 :type (unsigned-byte 16) :accessor ppu-address)
   (data :initform 0 :type octet :accessor ppu-data)
   (fine-x :initform 0 :type octet :accessor ppu-fine-x)
   (write-latch :initform nil :type boolean :accessor ppu-write-latch)
   (palette :initform (make-octet-vector #x20) :type octet-vector :reader ppu-palette)
   (oam :initform (make-octet-vector #x100) :type octet-vector :reader ppu-oam)
   (name-table :initarg :name-table :type octet-vector :reader ppu-name-table)
   (pattern-table :initarg :pattern-table :type mapper :reader ppu-pattern-table)))

(defun make-ppu (&key (name-table (make-octet-vector #x1000)) (cart (load-rom)))
  (make-instance 'ppu :name-table name-table :pattern-table cart))

(defun read-ppu (ppu address)
  (cond ((= address 2) ;; PPUSTATUS
         (read-status ppu))
        ((= address 4) ;; OAMDATA
         (aref (ppu-oam ppu) (ppu-oam-addr ppu)))
        ((= address 7) ;; PPUDATA
         (read-vram ppu))
        (t 0)))

(defun write-ppu (ppu address value)
  (cond ((= address 0) ;; PPUCTRL
         (setf (ppu-ctrl ppu) value)
         (setf (ldb (byte 2 10) (ppu-scroll ppu)) (ldb (byte 2 0) value)))
        ((= address 1) ;; PPUMASK
         (setf (ppu-mask ppu) value))
        ((= address 3) ;; OAMADDR
         (setf (ppu-oam-addr ppu) value))
        ((= address 4) ;; OAMDATA
         (write-oam ppu value))
        ((= address 5) ;; PPUSCROLL
         (write-scroll ppu value))
        ((= address 6) ;; PPUADDR
         (write-address ppu value))
        ((= address 7) ;; PPUDATA
         (write-vram ppu value))
        (t 0)))

(defun nametable-address (ppu)
  (let ((nt-index (ldb (byte 2 0) (ppu-ctrl ppu))))
    (ecase nt-index
      (0 #x2000)
      (1 #x2400)
      (2 #x2800)
      (3 #x2C00))))

(defun vram-increment (ppu)
  (if (logbitp 2 (ppu-ctrl ppu))
      32
      1))

(defun sprite-address (ppu)
  (* #x1000 (ldb (byte 1 3) (ppu-ctrl ppu))))

(defun background-address (ppu)
  (* #x1000 (ldb (byte 1 4) (ppu-ctrl ppu))))

(defun sprite-size (ppu)
  (if (logbitp 5 (ppu-ctrl ppu))
      :8x16
      :8x8))

(defun vblank-nmi? (ppu)
  (logbitp 7 (ppu-ctrl ppu)))

(defun set-vblank! (ppu value)
  (with-accessors ((status ppu-status)) ppu
    (setf (ldb (byte 1 7) status) value)))

(macrolet ((define-bit-test (function-name index)
               `(defun ,function-name (ppu)
                  (logbitp ,index (ppu-mask ppu)))))
  (define-bit-test grayscale? 0)
  (define-bit-test show-background-left? 1)
  (define-bit-test show-sprites-left? 2)
  (define-bit-test render-background? 3)
  (define-bit-test render-sprites? 4))

(defun rendering-enabled? (ppu)
  (or (render-background? ppu)
      (render-sprites? ppu)))

(defun read-status (ppu)
  (let ((result (ppu-status ppu)))
    (set-vblank! ppu 0)
    (setf (ppu-write-latch ppu) nil)
    result))

(defun read-vram (ppu)
  (with-accessors ((address ppu-address)
                   (cartridge ppu-pattern-table)) ppu
    (let ((buffer (ppu-data ppu))
          (new-value
            (cond ((< address #x2000)
                   (get-chr cartridge address))
                  ((< address #x3F00)
                   (aref (ppu-name-table ppu) (ldb (byte 12 0) address)))
                  (t
                   (read-palette ppu address)))))
      (setf (ppu-data ppu) new-value)
      (prog1 
          (if (< address #x3F00)
              buffer
              (read-palette ppu address))
        (incf address (vram-increment ppu))))))

(defun read-palette (ppu address)
  (aref (ppu-palette ppu) (palette-index address)))

(defun write-oam (ppu value)
  (with-accessors ((oam-addr ppu-oam-addr)) ppu
    (setf (aref (ppu-oam ppu) oam-addr) value)
    (setf oam-addr (wrap-byte (1+ oam-addr)))))

(defun write-scroll (ppu value)
  (with-accessors ((scroll ppu-scroll)
                   (fine-x ppu-fine-x)
                   (write-latch ppu-write-latch)) ppu
    (if (null write-latch)
        (setf (ldb (byte 5 0) scroll) (ldb (byte 5 3) value)
              fine-x (ldb (byte 3 0) value)
              write-latch t)
        (setf (ldb (byte 5 5) scroll) (ldb (byte 5 3) value)
              (ldb (byte 3 12) scroll) (ldb (byte 3 0) value)
              write-latch nil))))

(defun write-address (ppu value)
  (with-accessors ((scroll ppu-scroll)
                   (address ppu-address)
                   (write-latch ppu-write-latch)) ppu
    (if (null write-latch)
        (setf (ldb (byte 6 8) scroll) (ldb (byte 6 0) value)
              (ldb (byte 1 14) scroll) 0
              write-latch t)
        (setf (ldb (byte 8 0) scroll) value
              address scroll
              write-latch nil))))

(defun write-vram (ppu value)
  (with-accessors ((address ppu-address)
                   (palette ppu-palette)
                   (nametable ppu-name-table)
                   (cartridge ppu-pattern-table)) ppu
    (prog1
        (cond ((< address #x2000)
               (set-chr cartridge address value))
              ((< address #x3F00)
               (setf (aref nametable (ldb (byte 12 0) address)) value))
              (t
               (setf (aref palette (palette-index address)) value)))
      (incf address (vram-increment ppu)))))

(defun get-mirroring (ppu)
  (clones.mappers:mirroring (ppu-pattern-table ppu)))

(defun nt-mirror (ppu address)
  (let ((result (ecase (get-mirroring ppu)
                  (:horizontal (mod address #x800))
                  (:vertical (dpb 0 (byte 1 10) address)))))
    (ldb (byte 12 0) result)))

(defun palette-index (address)
  (let ((index (ldb (byte 5 0) address)))
    (if (and (zerop (mod index 4))
             (> index #x0F))
        (- index 16)
        index)))

(defun current-scanline (ppu)
  (let* ((address (ppu-address ppu))
         (coarse-y (ldb (byte 5 5) address))
         (fine-y (ldb (byte 3 12) address)))
    (+ (* coarse-y 8) fine-y)))

;;; Sprites

(defclass sprite ()
  ((sprite-x :initarg :sprite-x :type octet :reader sprite-x)
   (sprite-y :initarg :sprite-y :type octet :reader sprite-y)
   (pattern-index :initarg :pattern-index :type octet :reader sprite-index)
   (attributes :initarg :attributes :type octet :reader sprite-attributes)))

(define-printer sprite (sprite-x sprite-y pattern-index attributes)
                "X: ~2,'0X Y: ~2,'0X Index: ~2,'0X Attributes: ~8,'0B"
                sprite-x sprite-y pattern-index attributes)

(defun make-sprite (ppu sprite-bytes)
  (make-instance 'sprite :sprite-x (aref sprite-bytes 3)
                         :sprite-y (aref sprite-bytes 0)
                         :pattern-index (aref sprite-bytes 1)
                         :attributes (aref sprite-bytes 2)))

(defun evaluate-sprites (ppu scanline)
  (let ((visible-sprites (make-array 8 :initial-element nil))
        (current-sprite 0))
    (dotimes (sprite-index 64)
      (let ((candidate-y (aref (ppu-oam ppu) (* sprite-index 4))))
        (when (<= candidate-y scanline (+ candidate-y 7))
          (if (= current-sprite 8)
              (set-sprite-overflow! ppu 1)
              (let ((sprite-bytes (subseq (ppu-oam ppu) sprite-index (+ sprite-index 4))))
                (setf (aref visible-sprites current-sprite) (make-sprite ppu sprite-bytes)
                      current-sprite (1+ current-sprite)))))))
    visible-sprites))

(macrolet ((define-status-bit (function-name index)
             `(defun ,function-name (ppu value)
                (setf (ldb (byte 1 ,index) (ppu-status ppu)) value))))
  (define-status-bit set-sprite-overflow! 5)
  (define-status-bit set-sprite-zero-hit! 6))

;;; Graphics Fetching

(defun fetch-nt-byte (ppu)
  "See: https://www.nesdev.org/wiki/PPU_scrolling#Tile_and_attribute_fetching"
  (aref (ppu-name-table ppu) (nt-mirror ppu (ppu-address ppu))))

(defun fetch-at-byte (ppu)
  "See: https://www.nesdev.org/wiki/PPU_scrolling#Tile_and_attribute_fetching"
  (let* ((address (ppu-address ppu))
         (coarse-x-bits (ldb (byte 3 2) address))
         (coarse-y-bits (ldb (byte 3 7) address))
         (nt-select (ldb (byte 2 10) address))
         (attribute-offset #b1111)
         (at-index
           (~>> coarse-x-bits
                (dpb coarse-y-bits (byte 3 3))
                (dpb attribute-offset (byte 4 6))
                (dpb nt-select (byte 2 10)))))
    (aref (ppu-name-table ppu) (nt-mirror ppu at-index))))

(defgeneric palette-high-bits (ppu tile-descriptor)
  (:documentation "Determine the 2 high bits of the palette index for TILE-DESCRIPTOR."))

(defmethod palette-high-bits ((ppu ppu) (tile-descriptor fixnum))
  (let ((attributes (fetch-at-byte ppu)))
    (ecase (quad-position ppu)
      (:top-left (ldb (byte 2 0) attributes))
      (:top-right (ldb (byte 2 2) attributes))
      (:bottom-left (ldb (byte 2 4) attributes))
      (:bottom-right (ldb (byte 2 6) attributes)))))

(defmethod palette-high-bits ((ppu ppu) (tile-descriptor sprite))
  (let ((attributes (sprite-attributes tile-descriptor)))
    (ldb (byte 2 0) attributes)))

(defun palette-low-bits (low-byte high-byte index)
  (dpb (ldb (byte 1 (- 7 index)) high-byte)
       (byte 1 1)
       (ldb (byte 1 (- 7 index)) low-byte)))

(defgeneric find-pattern-index (ppu tile-descriptor)
  (:documentation "Find the index in the pattern table corresponding to TILE-DESCRIPTOR.
Return two values, the index including an offset for the current scanline and the base index."))

(defmethod find-pattern-index ((ppu ppu) (tile-descriptor fixnum))
  ;; See: https://www.nesdev.org/wiki/PPU_pattern_tables#Addressing
  (let* ((address (ppu-address ppu))
         (y-offset (ldb (byte 3 12) address))
         (bg-table (ldb (byte 1 4) (ppu-ctrl ppu)))
         (pattern-base (~>> 0
                            (dpb tile-descriptor (byte 8 4))
                            (dpb bg-table (byte 1 12)))))
    (values (+ pattern-base y-offset) pattern-base)))

(defmethod find-pattern-index ((ppu ppu) (tile-descriptor sprite))
  (let* ((offset (sprite-address ppu))
         (scanline (current-scanline ppu))
         (y-offset (- scanline (sprite-y tile-descriptor)))
         (pattern-base (+ offset (* 16 (sprite-index tile-descriptor)))))
    (values (+ pattern-base y-offset) pattern-base)))

(defun fetch-tile-bytes (ppu tile-descriptor)
  "Fetch all 16 bytes of the the pattern corresponding to TILE-DESCRIPTOR."
  (let ((pattern-index (nth-value 1 (find-pattern-index ppu tile-descriptor)))
        (pattern-table (ppu-pattern-table ppu)))
    (loop for i from 0 to 16
          collecting (clones.mappers:get-chr pattern-table (+ pattern-index i)))))

(defun fetch-scanline-bytes (ppu tile-descriptor)
  "Fetch the low and high bytes of the pattern matching TILE-DESCRIPTOR
   that are appropriate for display on the current scanline of PPU."
  (let ((pattern-table (ppu-pattern-table ppu))
        (pattern-index (find-pattern-index ppu tile-descriptor)))
    (values
     (clones.mappers:get-chr pattern-table pattern-index)
     (clones.mappers:get-chr pattern-table (+ pattern-index 8)))))

(defun quad-position (ppu)
  (let* ((address (ppu-address ppu))
         (on-right (logbitp 1 address))
         (on-bottom (logbitp 6 address)))
    (cond ((and on-right on-bottom)  :bottom-right)
          (on-bottom                 :bottom-left)
          (on-right                  :top-right)
          (t                         :top-left))))

;;; Scrolling

(defun coarse-scroll-horizontal! (ppu)
  "A scroll operation that conceptually occurs at the end of each 8-pixel tile."
  (symbol-macrolet ((nt-index (ldb (byte 1 11) (ppu-address ppu)))
                    (coarse-x (ldb (byte 5 0) (ppu-address ppu))))
    (cond ((= coarse-x 31)
           (setf coarse-x 0
                 nt-index (if (zerop nt-index) 1 0)))
          (t
           (incf coarse-x)))))

(defun fine-scroll-vertical! (ppu)
  "A scroll operation that conceptually occurs at the end of each scanline."
  (symbol-macrolet ((nt-index (ldb (byte 1 12) (ppu-address ppu)))
                    (coarse-y (ldb (byte 5 5) (ppu-address ppu)))
                    (fine-y (ldb (byte 3 12) (ppu-address ppu))))
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
