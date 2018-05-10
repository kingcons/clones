(in-package :cl-user)

(defpackage :clones-test.helpers
  (:use :cl :prove)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :alexandria
                :last-elt)
  (:export #:run-file))

(in-package :clones-test.helpers)

(defun run-file (filename)
  (let* ((test-file (concatenate 'string "t/" filename ".lisp"))
         (pathname (asdf/system:system-relative-pathname :clones test-file))
         (prove:*enable-colors* nil))
    (prove:run pathname :reporter :list)))

(defun parse-log (line)
  "Parse a line of nestest.log, returning a list of the form: (pc acc x y status stack cycles)"
  (flet ((unhexify (text)
           (parse-integer text :radix 16)))
    (loop for word in (delete #\Return (split-sequence #\Space line))
          collect (unhexify (last-elt (split-sequence #\: word))))))
