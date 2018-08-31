(in-package :cl-user)

(defpackage :clones-test.ppu
  (:use :cl :clones.ppu :prove))

(in-package :clones-test.ppu)

(defun test-ppu-construction ()
  (let ((ppu (make-ppu)))
    (is (type-of ppu) 'ppu)))

(plan 1)

(subtest "PPU Interface"
  (test-ppu-construction))

(finalize)
