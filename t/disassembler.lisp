(in-package :cl-user)

(defpackage :clones-test.disassembler
  (:use :cl :clones.disassembler :prove)
  (:import-from :clones.cpu :make-cpu :cpu-pc))

(in-package :clones-test.disassembler)

(plan 2)

(subtest "Basic Disassembly"
  (let ((cpu (make-cpu))
        (output (format nil "~A~%" "C000  4C F5 C5  ;; JMP $C5F5")))
    (clones.instruction-data:init-opcode-info)
    (setf (cpu-pc cpu) #xC000)
    (is-print (now cpu) output)))

(subtest "Implied mode"
  (let ((cpu (make-cpu))
        (output (format nil "~A~%" "C72D  EA        ;; NOP")))
    (clones.instruction-data:init-opcode-info)
    (setf (cpu-pc cpu) #xC72D)
    (is-print (now cpu) output)))

(finalize)
