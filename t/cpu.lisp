(in-package :cl-user)

(defpackage :clones-test.cpu
  (:use :cl :clones.cpu :prove)
  (:import-from :clones.instructions
                :single-step)
  (:import-from :clones.util
                :asset-path)
  (:import-from :clones-test.helpers
                :values-log))

(in-package :clones-test.cpu)

(defvar *perfect-log*
  (with-open-file (in (asset-path "roms/nestest_cpu.log"))
    (loop for line = (read-line in nil)
          until (string= (subseq line 0 4) "C6BD")
          collecting (values-log line))))

(defun run-legal-opcode-tests (cpu trace-asm)
  (setf (cpu-pc cpu) #xC000)
  (dolist (statement *perfect-log*)
    (loop for (accessor expected) in statement
          do (let ((value (funcall accessor cpu)))
               (unless (= value expected)
                 (fail (format t "Expected: ~A, Actual: ~A" expected value))
                 (return-from run-legal-opcode-tests nil))))
    (when trace-asm
      (clones.disassembler:now cpu))
    (single-step cpu))
  (pass "Completed all legal opcode tests in nestest!"))

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
  (let ((cpu (make-cpu))
        (trace-asm nil))
    (run-legal-opcode-tests cpu trace-asm)))

(finalize)
