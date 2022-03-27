(defpackage :clones.test.mappers
  (:use :cl :clones.mappers :try)
  (:export #:test-mappers))

(in-package :clones.test.mappers)

(deftest test-mappers ()
  (test-nrom))

(deftest test-nrom ()
  (let* ((nestest (asdf:system-relative-pathname :clones "roms/nestest.nes"))
         (mapper (load-rom nestest)))
    (is (eql (class-name (class-of mapper)) 'clones.mappers::nrom))
    (is (eql 76 (get-prg mapper 0)))
    (is (eql 76 (get-prg mapper 16384)))
    (set-prg mapper 0 0)
    (is (eql 76 (get-prg mapper 0)))))

#+nil
(try 'test-mappers)
