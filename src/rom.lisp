(in-package :cl-user)

(defpackage :clones.rom
  (:use :cl :alexandria :clones.util)
  (:export #:parse-rom
           #:rom))

(in-package :clones.rom)

(defstruct rom)

(defun parse-rom (pathname)
  (make-rom))
