(mgl-pax:define-package :clones
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.renderer)
  (:import-from :clones.ppu
                #:ppu
                #:make-ppu))

(in-package :clones)

(defun main ()
  )
