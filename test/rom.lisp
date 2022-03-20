(defpackage :clones.test.rom
  (:use :cl :clones.rom :try)
  (:export #:rom-tests))

(in-package :clones.test.rom)

(deftest rom-tests ()
  (is (= 2 2)))

#+nil
(try 'rom-tests)
