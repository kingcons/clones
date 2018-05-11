(in-package :cl-user)

(defpackage :clones.disassembler
  (:use :cl :clones.instruction-data)
  (:import-from :clones.memory
                :fetch
                :fetch-range)
  (:export :disasm))

(in-package :clones.disassembler)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *opcodes* (make-array 256 :element-type 'cons)
    "An array of opcode metadata for fast disassembly.")

  (dolist (metadata *instructions*)
    (destructuring-bind (name opcodes docs &optional skip-pc) metadata
      (declare (ignore skip-pc))
      (loop for (opcode bytes cycles mode) in opcodes
            do (setf (aref *opcodes* opcode)
                     (list name bytes cycles mode docs))))))

(defun hexify (bytes)
  (format nil "~{~2,'0x ~}" bytes))

(defun disasm (memory start end)
  (loop with index = start while (<= index end)
        for opcode = (fetch memory index)
        do (destructuring-bind (name size cycles mode docs) (aref *opcodes* opcode)
             (declare (ignore cycles mode docs))
             (let ((bytes (fetch-range memory index (+ index (1- size)))))
               (format t "~4,'0x  ~9a ;; ~a~%" index (hexify bytes) name))
             (incf index size))))
