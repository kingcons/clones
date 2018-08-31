(in-package :cl-user)

(defpackage :clones.ppu
  (:use :cl)
  (:import-from :clones.util
                :ub8
                :ub16
                :byte-vector
                :make-byte-vector)
  (:import-from :clones.mappers
                :mapper)
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
           #:emphasize-blue-p))

(in-package :clones.ppu)

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
