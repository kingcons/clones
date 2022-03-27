(defpackage :clones.test.rom
  (:use :cl :clones.rom :try)
  (:export #:test-rom))

(in-package :clones.test.rom)

(deftest test-rom ()
  (let ((invalid (asdf:system-relative-pathname :clones "roms/invalid.nes"))
        (nestest (asdf:system-relative-pathname :clones "roms/nestest.nes"))
        (mmc4 (asdf:system-relative-pathname :clones "roms/mmc4.nes")))
    (signals (invalid-rom)
      (parse-rom invalid))
    (signals-not (invalid-rom)
      (parse-rom nestest)
      (parse-rom mmc4))
    (is (subsetp '(:mapper-name :nrom :prg-count 1 :chr-count 1)
                 (parse-rom nestest)))
    (is (subsetp '(:mapper-name :unsupported)
                 (parse-rom mmc4)))))

#+nil
(try 'test-rom)
