(in-package :cl-user)

(defpackage :clones-test.helpers
  (:use :cl :prove)
  (:export #:run-file))

(in-package :clones-test.helpers)

(defun run-file (filename)
  (let* ((test-file (concatenate 'string "t/" filename ".lisp"))
         (pathname (asdf/system:system-relative-pathname :clones test-file))
         (prove:*enable-colors* nil))
    (prove:run pathname :reporter :list)))
