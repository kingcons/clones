(in-package :cl-user)

(defpackage :clones.disassembler
  (:use :cl :clones.addressing :clones.instruction-data)
  (:import-from :clones.memory
                :fetch
                :fetch-range)
  (:import-from :clones.cpu
                :cpu-memory
                :cpu-pc)
  (:export #:disasm
           #:now))

(in-package :clones.disassembler)

(defun disasm-bytes (index bytes)
  (format t "~4,'0x  ~9@<~{~2,'0x ~}~> ;; " index bytes))

(defun disasm-insts (name bytes addr-mode)
  (let* ((formatter (get-format-string addr-mode))
         (2-byte-modes '(absolute absolute-x absolute-y indirect))
         (opcode-args (if (member addr-mode 2-byte-modes)
                          (reverse (rest bytes))
                          (rest bytes))))
    (format t "~a~@[ ~@?~]~%" name formatter opcode-args)))

(defun disasm (memory start end)
  (loop with index = start
        while (<= index end)
        for opcode = (fetch memory index)
        for metadata = (aref *opcodes* opcode)
        do (with-slots (name byte-count addr-mode) metadata
             (let ((bytes (fetch-range memory index (+ index (1- byte-count)))))
               (disasm-bytes index bytes)
               (disasm-insts name bytes addr-mode)
               (incf index byte-count)))))

(defun current-instruction (memory start)
  (let* ((opcode (fetch memory start))
         (size (slot-value (aref *opcodes* opcode) 'byte-count)))
    (disasm memory start (+ start (1- size)))))

(defun now (cpu)
  (current-instruction (cpu-memory cpu) (cpu-pc cpu)))
