(mgl-pax:define-package :clones.disassembler
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.opcodes)
  (:import-from :clones.memory #:fetch))

(in-package :clones.disassembler)

(defsection @disassembler (:title "Disassembler")
  (disasm function))

(defun format-args (mode bytes)
  "Take a keyword representing a 6502 addressing mode
   and a list of bytes and format the arguments in the
   conventional 6502 assembly style."
  (let ((formatter
          (ecase mode
            (:immediate "#$~{~2,'0X~}")
            (:zero-page "$~{~2,0X~}")
            (:zero-page-x "$~{~2,'0X~}, X")
            (:zero-page-y "$~{~2,'0X~}, Y")
            (:accumulator "A")
            (:absolute  "$~{~2,'0X~}")
            (:absolute-x "$~{~2,'0X~}, X")
            (:absolute-y "$~{~2,'0X~}, Y")
            (:implied "")
            (:indirect "($~{~2,'0X~})")
            (:indirect-x "($~{~2,'0X~}, X)")
            (:indirect-y "($~{~2,'0X~}), Y")
            (:relative "$~{~2,0X~}")))
        (args (if (member mode '(:absolute :absolute-x :absolute-y :indirect))
                  (reverse bytes)
                  bytes)))
    (format nil formatter args)))

(defun disasm (memory start end)
  "Loop through MEMORY from START to END printing disassembly
   for each instruction found in the specified range. An error
   will be thrown if illegal instructions are present or if the
   start index is not the beginning of a 6502 instruction."
  (loop for index = start then (+ index length)
        for byte = (fetch memory index)
        for opcode = (aref *opcode-table* byte)
        for length = (opcode-size opcode)
        for bytes = (loop for i from index below (+ index length)
                          collect (fetch memory i))
        for mode = (opcode-addressing-mode opcode)
        until (> index end)
        do (format t "~4,'0X:  ~11@< ~{~2,'0X ~} ~> ;;  ~A ~A~%"
                   index
                   bytes
                   (opcode-name opcode)
                   (format-args mode (rest bytes)))))
