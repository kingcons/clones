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
           #:ppu-pattern-table))

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
