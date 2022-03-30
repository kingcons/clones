(mgl-pax:define-package :clones.disassembler
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.opcodes)
  (:import-from :clones.memory #:fetch))

(in-package :clones.disassembler)

(defsection @disassembler (:title "Disassembler")
  (disasm function))

(defun disasm (memory start end)
  "Loop through MEMORY from START to END printing disassembly
   for each instruction found in the specified range. An error
   will be thrown if illegal instructions are present or if the
   start index is not the beginning of a 6502 instruction."
  (let ((table (build-opcode-table)))
    (loop for index = start then (+ index length)
          for byte = (fetch memory index)
          for opcode = (aref table byte)
          for length = (opcode-size opcode)
          for bytes = (loop for i from index below (+ index length)
                           collect (fetch memory i))
          until (> index end)
          do (format t "~4,'0X:  ~11@< ~{~2,'0X ~} ~> ;;  ~A~%"
                     index
                     bytes
                     (opcode-name opcode)))))
