(mgl-pax:define-package :clones.cpu
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.opcodes :clones.memory)
  (:import-from :clones.disassembler #:disasm))

(in-package :clones.cpu)

(defsection @cpu (:title "CPU Core")
  (make-cpu function)
  (cpu-memory structure-accessor)
  (cpu-accum structure-accessor)
  (cpu-x structure-accessor)
  (cpu-y structure-accessor)
  (cpu-status structure-accessor)
  (cpu-stack structure-accessor)
  (cpu-pc structure-accessor)
  (cpu-cycles structure-accessor)
  (single-step function)
  (now function))

(define-condition addressing-mode-not-implemented (error)
  ((opcode :initarg :opcode :reader error-opcode)
   (mode :initarg :mode :reader error-mode))
  (:report (lambda (condition stream)
             (format stream "Could not find Addressing Mode ~S for Opcode ~A"
                     (error-mode condition)
                     (error-opcode condition)))))

(define-condition opcode-not-implemented (error)
  ((opcode :initarg :opcode :reader error-opcode))
  (:report (lambda (condition stream)
             (format stream "Could not find handler for Opcode ~A"
                     (error-opcode condition)))))

(define-condition illegal-opcode (error)
  ((opcode :initarg :opcode :reader error-opcode))
  (:report (lambda (condition stream)
             (format stream "Clones does not support Illegal Opcode ~A"
                     (error-opcode condition)))))

(defstruct cpu
  (memory (make-memory) :type memory)
  (accum 0 :type (unsigned-byte 8))
  (x 0 :type (unsigned-byte 8))
  (y 0 :type (unsigned-byte 8))
  (status #x24 :type (unsigned-byte 8))
  (stack #xFD :type (unsigned-byte 8))
  (pc #xFFFC :type (unsigned-byte 16))
  (cycles 0 :type fixnum))

(defun now (cpu)
  "Disassemble the current instruction pointed to by the CPU's program counter."
  (with-accessors ((memory cpu-memory)
                   (pc cpu-pc)) cpu
    (let ((opcode (aref *opcode-table* (fetch memory pc))))
      (disasm memory pc (+ pc (1- (opcode-size opcode)))))))

(defun get-operand (cpu opcode)
  (flet ((get-address (addressing-mode)
           (case addressing-mode
             (otherwise (error 'addressing-mode-not-implemented
                               :mode addressing-mode
                               :opcode opcode)))))
    (let ((address (get-address (opcode-addressing-mode opcode)))
          (access-pattern (opcode-access-pattern opcode)))
      (ecase access-pattern))))

(defun single-step (cpu)
  "Step the CPU over the current instruction."
  (with-accessors ((pc cpu-pc)
                   (cycles cpu-cycles)
                   (memory cpu-memory)) cpu
    (let* ((byte (fetch memory pc))
           (opcode (aref *opcode-table* byte))
           (handler (opcode-name opcode))
           (operand (get-operand cpu opcode)))
      (when (eql handler :illegal)
        (error 'illegal-opcode :opcode opcode))
      (unless (fboundp handler)
        (error 'opcode-not-implemented :opcode opcode))
      (funcall handler cpu operand)
      ;; Update the program counter and cycle count
      (incf cycles (opcode-time opcode))
      (unless (eql (opcode-access-pattern opcode) :jump)
        (incf pc (opcode-size opcode))))))
