(in-package :cl-user)

(defpackage clones-test.rom
  (:use :cl :clones.rom :clones.util :prove))

(in-package :clones-test.rom)

(plan nil)

(let ((nestest (clones-asset "roms/nestest.nes")))
  (ok (typep (parse-rom nestest) 'rom)))

(finalize)

