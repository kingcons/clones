(in-package :cl-user)

(defpackage clones-test.rom
  (:use :cl :clones.rom :clones.util :prove))

(in-package :clones-test.rom)

(plan 1)

(subtest "ROM Interface"
  (let* ((nestest (clones-asset "roms/nestest.nes"))
         (rom (parse-rom nestest))
         (header (rom-header rom)))
    (is-type rom 'rom)
    ;; Use subseq here to avoid printing out the whole byte vector in test report.
    (is-type (subseq (rom-prg rom) 0 #x20) 'byte-vector)
    (is-type (subseq (rom-chr rom) 0 #x20) 'byte-vector)
    (is (rom-pathname rom) nestest)
    (is (getf header :prg-count) 1)
    (is (getf header :chr-count) 1)
    (is (getf header :mirroring) :horizontal)
    (is (getf header :mapper-name) :nrom)
    (is-print (princ rom) "#<ROM nestest.nes :prg-size 16384 :chr-size 8192 :mapper-name NROM>")))

(finalize)

