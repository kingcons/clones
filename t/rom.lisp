(in-package :cl-user)

(defpackage clones-test.rom
  (:use :cl :clones.rom :prove))

(in-package :clones-test.rom)

(plan 1)

(subtest "ROM Interface"
  (let* ((nestest (clones.util:asset-path "roms/nestest.nes"))
         (invalid (clones.util:asset-path "roms/invalid.nes"))
         (aorom (clones.util:asset-path "roms/aorom.nes"))
         (rom (parse-rom nestest)))
    (is-error (parse-rom invalid) 'clones.conditions:invalid-rom)
    (is (rom-mapper-name (parse-rom aorom)) "Unknown")
    (is-type rom 'rom)
    ;; Use subseq here to avoid printing out the whole byte vector in test.
    (is-type (subseq (rom-prg rom) 0 #x10) 'clones.util:byte-vector)
    (is-type (subseq (rom-chr rom) 0 #x10) 'clones.util:byte-vector)
    (is (rom-pathname rom) nestest)
    (is (rom-prg-count rom) 1)
    (is (rom-chr-count rom) 1)
    (is (rom-prg-size rom) #x4000)
    (is (rom-chr-size rom) #x2000)
    (is (rom-mirroring rom) :horizontal)
    (is (rom-mapper-name rom) :nrom)
    (is-print (princ rom) "#<ROM nestest.nes :prg-size 16384 :chr-size 8192 :mapper-name NROM>")))

(finalize)
