(mgl-pax:define-package :clones
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.renderer)
  (:import-from :clones.ppu
                #:ppu
                #:make-ppu))

(in-package :clones)

(defun main ()
  (let* ((cpu (make-cpu))
         (ppu (memory-ppu (cpu-memory cpu)))
         (renderer (make-renderer :ppu ppu
                                  :on-nmi (lambda () (nmi cpu)))))
    (loop until (> (cpu-cycles cpu) +cycles-per-scanline+)
          do (progn
               (single-step cpu)))))
