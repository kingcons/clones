(in-package :cl-user)

(defpackage clones-test.rom
  (:use :cl :clones.rom :clones.util :prove))

(in-package :clones-test.rom)

(plan 1)

(subtest "ROM Interface"
  (let* ((nestest (clones-asset "roms/nestest.nes"))
         (rom (parse-rom nestest)))
    (is-type rom 'rom)
    ;; Use subseq here to avoid printing out the whole byte vector in test.
    (is-type (subseq (rom-prg rom) 0 #x10) 'byte-vector)
    (is-type (subseq (rom-chr rom) 0 #x10) 'byte-vector)
    (is (rom-pathname rom) nestest)
    (is (rom-prg-count rom) 1)
    (is (rom-chr-count rom) 1)
    (is (rom-mirroring rom) :horizontal)
    (is (rom-mapper-name rom) :nrom)
    (is-print (princ rom) "#<ROM nestest.nes :prg-size 16384 :chr-size 8192 :mapper-name NROM>")))

(finalize)
