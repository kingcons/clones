(in-package :cl-user)

(defpackage :clones-test.ppu
  (:use :cl :clones.ppu :prove))

(in-package :clones-test.ppu)

(defun test-ppu-construction ()
  (let ((ppu (make-ppu)))
    (is (type-of ppu) 'ppu)))

(defun test-ppu-registers-init ()
  (let ((ppu (make-ppu)))
    (is (ppu-control ppu) 0)
    (is (ppu-mask ppu) 0)
    (is (ppu-status ppu) 0)
    (is (ppu-oam-address ppu) 0)
    (is (ppu-address ppu) 0)
    (is (ppu-data ppu) 0)
    (is (ppu-coarse-x ppu) 0)
    (is (ppu-coarse-y ppu) 0)
    (is (ppu-nametable ppu) 0)
    (is (ppu-fine-x ppu) 0)
    (is (ppu-fine-y ppu) 0)))

(plan 1)

(subtest "PPU Interface"
  (test-ppu-construction)
  (test-ppu-registers-init))

(finalize)
