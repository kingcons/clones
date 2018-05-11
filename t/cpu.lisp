(in-package :cl-user)

(defpackage :clones-test.cpu
  (:use :cl :clones.cpu :prove)
  (:import-from :clones.instructions
                :single-step)
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

(plan 2)

(subtest "CPU Interface"
  (let ((cpu (make-cpu)))
    (is (type-of cpu) 'cpu)
    (is (cpu-cycles cpu) 0)
    (is (cpu-accum cpu) 0)
    (is (cpu-x-reg cpu) 0)
    (is (cpu-y-reg cpu) 0)
    (is (cpu-stack cpu) 253)
    (is (cpu-status cpu) 36)
    (is (cpu-pc cpu) #xfffc)
    (subtest "The CPU jumps to the Reset Vector when Reset"
      (reset cpu)
      (is (cpu-pc cpu) 49156))))

(subtest "Nestest.nes"
  (let* ((cpu (make-cpu)))
    (setf (cpu-pc cpu) #xC000)
    (with-open-file (in (asset-path "roms/nestest_cpu.log"))
      (loop for line = (read-line in nil) while line
            do (let ((log (debug-log cpu))
                     (expected (parse-log line)))
                 (unless (equal log expected)
                   (fail (format t "Expected: ~A, Actual: ~A" expected log))
                   (return nil))
                 (single-step cpu))))))

(finalize)
