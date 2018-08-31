(in-package :cl-user)

(defpackage :clones.ppu
  (:use :cl)
  (:import-from :clones.util :ub8
                             :ub16)
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
           #:ppu-nametable))

(in-package :clones.ppu)

(defstruct ppu
  (control      0 :type ub8)  ;; 0x2000
  (mask         0 :type ub8)  ;; 0x2001
  (status       0 :type ub8)  ;; 0x2002
  (oam-address  0 :type ub8)  ;; 0x2003/0x2004
  (address      0 :type ub16) ;; 0x2006
  (data         0 :type ub8)  ;; 0x2007
  (coarse-x     0 :type ub8)  ;; 0x2005
  (coarse-y     0 :type ub8)  ;; 0x2005
  (fine-x       0 :type ub8)  ;; 0x2005
  (fine-y       0 :type ub8)  ;; 0x2005
  (nametable    0 :type ub8)) ;; 0x2005
