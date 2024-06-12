(defpackage :clones.test
  (:use :cl :clones :try)
  (:export #:test-ci
           #:test-all))

(in-package :clones.test)

(deftest test-all ()
  (clones.test.rom:test-rom)
  (clones.test.mappers:test-mappers)
  (clones.test.ppu:test-ppu)
  (clones.test.memory:test-memory)
  (clones.test.cpu:test-cpu)
  (clones.test.renderer:test-renderer))

(defun test (&key (debug nil) (print 'unexpected) (describe 'unexpected))
  (warn-on-tests-not-run ((find-package :clones-test))
    (print (try 'test-all :debug debug :print print :describe describe))))

(defun test-ci ()
  (unless (try:passedp (try:try 'test-all))
    (uiop:quit 1)))

#+nil
(test)
