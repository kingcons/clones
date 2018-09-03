(in-package :cl-user)

(defpackage :clones-test.helpers
  (:use :cl :prove)
  (:import-from :prove.reporter.list
                :format-report)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :alexandria
                :last-elt
                :compose)
  (:export #:run-file))

(in-package :clones-test.helpers)

(defun patch-test-reporting ()
  (defmethod format-report (stream (reporter prove.reporter.list:list-reporter)
                            (report prove.report:passed-test-report) &rest args)
    (declare (ignore args))))

(defun drop-reporting-patch ()
  (let* ((specializers (mapcar #'find-class '(prove.reporter.list:list-reporter
                                              prove.report:passed-test-report)))
         (method (find-method #'format-report '() (cons t specializers))))
        (when method
          (remove-method #'format-report method))))

(defun run-file (filename)
  (let* ((test-file (concatenate 'string "t/" filename ".lisp"))
         (pathname (asdf/system:system-relative-pathname :clones test-file))
         (prove:*enable-colors* (if (find-package :swank-repl) nil t))
         (prove:*debug-on-error* (if (find-package :swank-repl) t nil)))
    (patch-test-reporting)
    (prove:run pathname :reporter :list)
    (drop-reporting-patch)))

(defun parse-log (line)
  "Parse a line of nestest.log, returning a list of the form: (pc acc x y status stack cycles)"
  (flet ((to-fixnum (text &key (radix 16))
           (parse-integer text :radix radix))
         (colon-value (field)
           (last-elt (split-sequence #\: field))))
    (let ((fields (split-sequence #\Space (delete #\Return line))))
      (append (mapcar (compose #'to-fixnum #'colon-value) (butlast fields))
              (list (to-fixnum (colon-value (last-elt fields)) :radix 10))))))

(defun values-log (line)
  (destructuring-bind (pc accum x y status stack cycles) (parse-log line)
    `((clones.cpu:cpu-pc ,pc)
      (clones.cpu:cpu-accum ,accum)
      (clones.cpu:cpu-x-reg ,x)
      (clones.cpu:cpu-y-reg ,y)
      (clones.cpu:cpu-status ,status)
      (clones.cpu:cpu-stack ,stack)
      (clones.cpu:cpu-cycles ,cycles))))
