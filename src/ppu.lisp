(mgl-pax:define-package :clones.ppu
  (:use :cl :alexandria :mgl-pax)
  (:import-from :serapeum #:octet))

(in-package :clones.ppu)

(defsection @ppu (:title "Picture Processing Unit")
  (ppu class)
  (make-ppu function)
  (read-ppu function)
  (write-ppu function))

(defstruct ppu
  (ctrl 0 :type octet)
  (mask 0 :type octet)
  (status 0 :type octet)
  (oam-addr 0 :type octet)
  (scroll 0 :type (unsigned-byte 16))
  (address 0 :type (unsigned-byte 16))
  (data 0 :type octet))

(defun read-ppu (ppu address)
  address)

(defun write-ppu (ppu address value)
  value)
