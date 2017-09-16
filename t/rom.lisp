(in-package :cl-user)

(defpackage clones-test.rom
  (:use :cl :clones.rom :clones.util :prove))

(in-package :clones-test.rom)

(plan nil)

(let* ((nestest (clones-asset "roms/nestest.nes"))
       (rom (parse-rom nestest)))
  (is (type-of rom) 'rom)
  (subtest "ROM Interface"
    (is (type-of (rom-binary rom)) byte-vector)
    (is (type-of (rom-header rom)) property-list)
    (is (type-of (rom-prg rom)) byte-vector)
    (is (type-of (rom-chr rom)) byte-vector)))

(finalize)

