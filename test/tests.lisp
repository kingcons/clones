(defpackage :clones.test
  (:use :cl :clones :try)
  (:export #:test-all))

(in-package :clones.test)

(deftest test-all ()
  (is (= 2 2)))

(defun test (&key (debug nil) (print 'unexpected) (describe 'unexpected))
  (warn-on-tests-not-run ((find-package :clones-test))
    (print (try 'test-all :debug debug :print print :describe describe))))

#+nil
(test)
