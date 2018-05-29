(in-package :cl-user)

(defpackage :clones.disassembler
  (:use :cl :clones.instruction-data)
  (:import-from :clones.memory
                :fetch
                :fetch-range)
  (:import-from :clones.cpu
                :memory
                :pc)
  (:export #:disasm
           #:now))

(in-package :clones.disassembler)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *opcodes* (make-array 256 :element-type 'cons)
    "An array of opcode metadata for fast disassembly.")

  (dolist (metadata *instructions*)
    (destructuring-bind (name opcodes docs &key access-pattern skip-pc) metadata
      (declare (ignore access-pattern skip-pc))
      (loop for (opcode bytes cycles mode) in opcodes
            do (setf (aref *opcodes* opcode)
                     (list name bytes cycles mode docs))))))

(defun disasm (memory start end)
  (loop with index = start while (<= index end)
        for opcode = (fetch memory index)
        do (destructuring-bind (name size cycles mode docs) (aref *opcodes* opcode)
             (declare (ignore cycles docs))
             (flet ((hexify (bytes)
                      (format nil "~{~2,'0x ~}" bytes))
                    (format-args (format-string bytes)
                      (if (member mode '(absolute absolute-x absolute-y indirect))
                          (format nil format-string (reverse bytes))
                          (format nil format-string bytes))))
               (let ((writer (clones.addressing:get-format-string mode))
                     (bytes (fetch-range memory index (+ index (1- size)))))
                 (if (rest bytes)
                     (format t "~4,'0x  ~9a ;; ~a ~a~%" index (hexify bytes)
                             name (format-args writer (rest bytes)))
                     (format t "~4,'0x  ~9a ;; ~a~%" index (hexify bytes) name))))
             (incf index size))))

(defun now (cpu)
  (with-slots (pc memory) cpu
    (let* ((opcode (fetch memory pc))
           (size (second (aref *opcodes* opcode))))
      (disasm memory pc (+ pc (1- size))))))
