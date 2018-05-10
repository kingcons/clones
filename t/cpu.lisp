(in-package :cl-user)

(defpackage :clones-test.cpu
  (:use :cl :clones.cpu :prove)
  (:import-from :clones.util
                :asset-path)
  (:import-from :clones-test.helpers
                :parse-log))

(in-package :clones-test.cpu)

(defun debug-log (cpu)
  (declare (type cpu cpu))
  (list (cpu-pc cpu)
        (cpu-accum cpu)
        (cpu-x-reg cpu)
        (cpu-y-reg cpu)
        (cpu-status cpu)
        (cpu-stack cpu)
        (cpu-cycles cpu)))

(defun confirm-state (cpu expected-state)
  (step cpu)
  (let ((log (debug-log cpu)))
    (unless (equal log expected-state)
      (fail (concatenate 'string "Failure at: " expected-state)))))

(plan 2)

(subtest "CPU Interface"
   (let* ((cpu (make-cpu)))
     (is (type-of cpu) 'cpu)
     (is (cpu-cycles cpu) 0)
     (is (cpu-accum cpu) 0)
     (is (cpu-x-reg cpu) 0)
     (is (cpu-y-reg cpu) 0)
     (is (cpu-stack cpu) 253)
     (is (cpu-status cpu) 36)
     (is (cpu-pc cpu) 65532)))

;; (subtest "Nestest.nes"
;;   (let* ((cpu (make-cpu)))
;;     (with-open-file (in (asset-path "roms/nestest_cpu.log"))
;;       (loop for line = (read-line in nil)
;;             while line
;;             do (confirm-state cpu (parse-log line))))))

(finalize)
