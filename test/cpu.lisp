(defpackage :clones.test.cpu
  (:use :cl :clones.cpu :try)
  (:export #:test-cpu)
  (:import-from :serapeum
                #:~>>
                #:op))

(in-package :clones.test.cpu)

(deftest test-cpu ()
  (test-initial-values)
  (test-legal-opcodes))

(deftest test-initial-values ()
  (let ((cpu (make-cpu)))
    (is (typep (cpu-memory cpu) 'clones.memory:memory))
    (is (zerop (cpu-accum cpu)))
    (is (zerop (cpu-x cpu)))
    (is (zerop (cpu-y cpu)))
    (is (= (cpu-status cpu) #b00100100))
    (is (= (cpu-stack cpu) #xFD))
    (is (= (cpu-pc cpu) #xFFFC))))

(deftest test-legal-opcodes ()
  (let ((cpu (make-cpu))
        (disassemble? nil)
        (perfect-log
          (~>> "roms/nestest_cpu.log"
               (asdf:system-relative-pathname :clones)
               (alexandria:read-file-into-string)
               (split-sequence:split-sequence #\Newline)
               (map 'vector (op (string-trim '(#\Return) _))))))
    (setf (cpu-pc cpu) #xC000)
    (loop for count = 0 then (1+ count)
          for percent = (* 100 (/ count 5002.0))
          for line = (aref perfect-log count)
          until (= (cpu-pc cpu) #xC6BD) ; First Illegal Opcode
          do (let ((expected line)
                   (actual (debug-log cpu)))
               (when disassemble?
                 (let ((state (now cpu :stream nil)))
                   (format t "~2,2$% | ~A" percent state)))
               (assert (string-equal expected actual))
               (single-step cpu))
          finally (is (= (cpu-pc cpu) #xC6BD)))))

(deftest test-opcodes-do-not-allocate ()
  (let ((cpu (make-cpu)))
    (setf (cpu-pc cpu) #xC000)
    (let ((bytes-allocated (sb-ext:get-bytes-consed)))
      (loop until (= (cpu-pc cpu) #xC6BD) ; First Illegal Opcode
            do (single-step cpu))
      (setf (cpu-pc cpu) #xC000
            (cpu-status cpu) #x24
            (cpu-stack cpu) #xFD)
      (is (= bytes-allocated (sb-ext:get-bytes-consed))))))

(defun debug-log (cpu)
  (with-accessors ((pc cpu-pc)
                   (accum cpu-accum)
                   (x-reg cpu-x)
                   (y-reg cpu-y)
                   (status cpu-status)
                   (stack cpu-stack)
                   (cycles cpu-cycles)) cpu
    (format nil "~4,'0X A:~2,'0X X:~2,'0X Y:~2,'0X P:~2,'0X SP:~2,'0X CPUC:~D"
            pc accum x-reg y-reg status stack cycles)))

#+nil
(let ((*print* 'unexpected))
  (test-legal-opcodes))
