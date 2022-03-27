(mgl-pax:define-package :clones.rom
  (:use :cl :alexandria :serapeum :mgl-pax))

(in-package :clones.rom)

(defsection @rom (:title "ROM Parsing")
  (valid-rom? function))

(defun valid-rom? (stream)
  (let ((header #(78 69 83 26))
        (bytes (make-array 4)))
    (read-sequence bytes stream)
    (equalp bytes header)))
