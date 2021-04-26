(in-package :cl-user)

(defpackage :clones-test.disassembler
  (:use :cl :clones.disassembler :prove)
  (:import-from :clones.cpu :make-cpu :cpu-pc))

(in-package :clones-test.disassembler)

(plan 1)

(subtest "Basic Disassembly"
  (let ((cpu (make-cpu))
        (output (format nil "~A~%" "C000  4C F5 C5  ;; JMP $F5C5")))
    (setf (cpu-pc cpu) #xC000)
    (is-print (now cpu) output)))

(finalize)
