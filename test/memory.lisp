(defpackage :clones.test.memory
  (:use :cl :clones.memory :try)
  (:export #:test-memory))

(in-package :clones.test.memory)

(deftest test-memory ()
  (test-ram)
  (test-cartridge))

(deftest test-ram ()
  (let ((memory (make-memory)))
    (is (eql 42 (store memory #x7ff 42)))
    (is (eql 42 (fetch memory #x7ff)))
    (is (eql 42 (fetch memory #x1fff)))))

(deftest test-cartridge ()
  (let ((memory (make-memory)))
    (is (eql 76 (fetch memory #x8000)))
    (is (eql 245 (fetch memory #x8001)))))

#+nil
(try 'test-memory)
