(defpackage :clones.test.cpu
  (:use :cl :clones.cpu :try)
  ;; Pull in some private symbols for inspecting state
  (:import-from :clones.cpu
                #:cpu-accum
                #:cpu-x
                #:cpu-y
                #:cpu-status
                #:cpu-stack
                #:cpu-pc
                #:cpu-cycles)
  (:import-from :serapeum
                #:~>>
                #:op)
  (:export #:test-cpu))

(in-package :clones.test.cpu)

(deftest test-cpu ()
  (test-initial-values)
  (test-legal-opcodes)
  (test-nmi)
  (test-backwards-branch))

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

(deftest test-nmi ()
  (let ((cpu (make-cpu)))
    (setf (cpu-pc cpu) #xC000)
    (dotimes (i 100)
      (single-step cpu))
    (let ((return-address (cpu-pc cpu))
          (cpu-status (cpu-status cpu)))
      (nmi cpu)
      (is (equal (cpu-pc cpu) (clones.memory:fetch-word (cpu-memory cpu) #xFFFA)))
      (is (equal cpu-status (clones.cpu::stack-pop cpu)))
      (is (equal return-address (clones.cpu::stack-pop-word cpu))))))

(deftest test-backwards-branch ()
  (let ((cpu (make-cpu)))
    (setf (cpu-pc cpu) #xC009) ;; LDA $2002
    (single-step cpu)
    (is (= (cpu-pc cpu) #xC00C)) ;; BPL &FB
    (single-step cpu)
    (is (= (cpu-pc cpu) #xC009))))

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
