(mgl-pax:define-package :clones.disassembler
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.opcodes)
  (:import-from :clones.memory
                #:fetch))

(in-package :clones.disassembler)

(defsection @disassembler (:title "Disassembler")
  (disasm function)
  (disassemble-instruction function))

(defun format-args (mode args)
  "Take a keyword representing a 6502 addressing mode
   and a list of ARGS and format the arguments in the
   conventional 6502 assembly style."
  (let ((formatter
          (ecase mode
            (:immediate "#$~{~2,'0X~}")
            (:zero-page "$~{~2,'0X~}")
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
            (:relative "$~{~2,'0X~}")))
        (ordered (if (member mode '(:absolute :absolute-x :absolute-y :indirect))
                     (reverse args)
                     args)))
    (format nil formatter ordered)))

(defun disassemble-instruction (memory index &key (stream t))
  "Disassemble a single instruction from MEMORY beginning at INDEX.
   STREAM is the FORMAT destination of the disassembly output."
  (let* ((opcode (find-opcode (fetch memory index)))
         (code (opcode-code opcode))
         (size (opcode-size opcode))
         (mode (opcode-addressing-mode opcode))
         (args (loop for i from (1+ index) below (+ index size)
                     collect (fetch memory i))))
    (values (format stream "~4,'0X:  ~11@< ~{~2,'0X ~} ~> ;;  ~A ~A~%"
                    index
                    (cons code args)
                    (opcode-name opcode)
                    (format-args mode args))
            size)))

(defun disasm (memory start end)
  "Loop through MEMORY from START to END printing disassembly
   for each instruction found in the specified range. An error
   will be thrown if illegal instructions are present or if the
   start index is not the beginning of a 6502 instruction."
  (loop for index = start then (+ index size)
        for size = (nth-value 1 (disassemble-instruction memory index))
        while (< index end)))
