(in-package :cl-user)

(defpackage :clones.disassembler
  (:use :cl :clones.instruction-data)
  (:import-from :clones.memory
                :fetch
                :fetch-range)
  (:import-from :clones.cpu
                :cpu-memory
                :cpu-pc)
  (:export #:disasm
           #:now))

(in-package :clones.disassembler)

(defun hexify (bytes)
  (format nil "~{~2,'0x ~}" bytes))

(defun disasm (memory start end)
  (loop with index = start while (<= index end)
        for opcode = (fetch memory index)
        do (with-slots (name byte-count addr-mode) (aref *opcodes* opcode)
             (flet ((format-args (format-string bytes)
                      (if (member addr-mode '(absolute absolute-x absolute-y indirect))
                          (format nil format-string (reverse bytes))
                          (format nil format-string bytes))))
               (let* ((writer (clones.addressing:get-format-string addr-mode))
                      (bytes (fetch-range memory index (+ index (1- byte-count))))
                      (args (format-args writer (rest bytes))))
                 (format t "~4,'0x  ~9a ;; ~a ~a~%" index (hexify bytes) name args)))
             (incf index byte-count))))

(defun current-instruction (memory start)
  (let* ((opcode (fetch memory start))
         (size (slot-value (aref *opcodes* opcode) 'byte-count)))
    (disasm memory start (+ start (1- size)))))

(defun now (cpu)
  (current-instruction (cpu-memory cpu) (cpu-pc cpu)))
