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

(define-condition access-pattern-not-implemented (error)
  ((opcode :initarg :opcode :reader error-opcode)
   (access-pattern :initarg :access-pattern :reader error-pattern))
  (:report (lambda (condition stream)
             (format stream "Could not find Access Pattern ~S for Opcode ~A"
                     (error-pattern condition)
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
  (with-accessors ((memory cpu-memory)
                   (pc cpu-pc)) cpu
    (flet ((get-address (addressing-mode)
             (case addressing-mode
               (:implied nil)
               (:immediate (1+ pc))
               (:zero-page (fetch memory (1+ pc)))
               (:absolute (let ((low-byte (fetch memory (1+ pc)))
                                (high-byte (fetch memory (+ pc 2))))
                            (+ (ash high-byte 8) low-byte)))
               (:relative (let* ((start (+ pc 2)) ; Instruction after the branch
                                 (offset (fetch memory (1+ pc)))
                                 (destination
                                   (if (logbitp 7 offset)
                                       (- start (logand offset #x7F))
                                       (+ start offset))))
                            destination))
               (otherwise (error 'addressing-mode-not-implemented
                                 :mode addressing-mode
                                 :opcode opcode)))))
      (let ((address (get-address (opcode-addressing-mode opcode)))
            (access-pattern (opcode-access-pattern opcode)))
        (case access-pattern
          (:read (fetch memory address))
          (:write address)
          (:jump address)
          (:static address)
          (otherwise (error 'access-pattern-not-implemented
                            :access-pattern access-pattern
                            :opcode opcode)))))))

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
      (unless (eql (opcode-access-pattern opcode) :jump)
        (incf pc (opcode-size opcode)))
      (incf cycles (opcode-time opcode)))))

(defmacro branch-if (cpu condition destination)
  (with-gensyms (start pc)
    `(with-accessors ((,pc cpu-pc)) ,cpu
       (let ((,start ,pc))
         (if ,condition
             (progn
               (setf ,pc ,destination)
               (if (page-crossed? ,start ,destination)
                   (incf (cpu-cycles ,cpu) 2)
                   (incf (cpu-cycles ,cpu))))
             (incf ,pc 2))))))

(defun page-crossed? (start dest)
  (not (= (ash start -8)
          (ash dest -8))))

(defun set-flag-zn (cpu operand)
  (with-accessors ((status cpu-status)) cpu
    (let ((zero-bit (if (zerop operand) 1 0))
          (sign-bit (if (logbitp 7 operand) 1 0)))
      (setf status (dpb sign-bit (byte 1 7)
                        (dpb zero-bit (byte 1 1) status))))))

(defun stack-pop-address (cpu)
  (with-accessors ((memory cpu-memory)
                   (stack cpu-stack)) cpu
    (let ((low-byte (fetch memory (1+ stack)))
          (high-byte (fetch memory (+ stack 2))))
      (incf stack 2)
      (+ (ash high-byte 8) low-byte))))

(defun stack-push-address (cpu address)
  (with-accessors ((memory cpu-memory)
                   (stack cpu-stack)) cpu
    (let ((low-byte (logand address #xFF))
          (high-byte (ash address -8)))
      (store memory stack high-byte)
      (decf stack)
      (store memory stack low-byte)
      (decf stack))))

(defun :bcc (cpu operand)
  (branch-if cpu (not (logbitp 0 (cpu-status cpu))) operand))

(defun :bcs (cpu operand)
  (branch-if cpu (logbitp 0 (cpu-status cpu)) operand))

(defun :beq (cpu operand)
  (branch-if cpu (logbitp 1 (cpu-status cpu)) operand))

(defun :bit (cpu operand)
  (let ((result (logand (cpu-accum cpu) operand)))
    (with-accessors ((status cpu-status)) cpu
      (setf status (dpb (ash operand -6) (byte 2 6) status))
      (setf status (dpb (if (zerop result) 1 0)
                        (byte 1 1)
                        status)))))

(defun :bne (cpu operand)
  (branch-if cpu (not (logbitp 1 (cpu-status cpu))) operand))

(defun :bpl (cpu operand)
  (branch-if cpu (not (logbitp 7 (cpu-status cpu))) operand))

(defun :bvc (cpu operand)
  (branch-if cpu (not (logbitp 6 (cpu-status cpu))) operand))

(defun :bvs (cpu operand)
  (branch-if cpu (logbitp 6 (cpu-status cpu)) operand))

(defun :clc (cpu operand)
  (declare (ignore operand))
  (with-accessors ((status cpu-status)) cpu
    (setf status (logand status #b11111110))))

(defun :jmp (cpu operand)
  (setf (cpu-pc cpu) operand))

(defun :jsr (cpu operand)
  (let ((return-address (+ (cpu-pc cpu) 2)))
    (stack-push-address cpu return-address)
    (setf (cpu-pc cpu) operand)))

(defun :lda (cpu operand)
  (setf (cpu-accum cpu) operand)
  (set-flag-zn cpu operand))

(defun :ldx (cpu operand)
  (setf (cpu-x cpu) operand)
  (set-flag-zn cpu operand))

(defun :nop (cpu operand)
  (declare (ignore cpu operand)))

(defun :rts (cpu operand)
  (declare (ignore operand))
  (let ((return-address (stack-pop-address cpu)))
    (setf (cpu-pc cpu) return-address)))

(defun :sec (cpu operand)
  (declare (ignore operand))
  (with-accessors ((status cpu-status)) cpu
    (setf status (logior status #b00000001))))

(defun :sta (cpu operand)
  (with-accessors ((memory cpu-memory)) cpu
    (store memory operand (cpu-accum cpu))))

(defun :stx (cpu operand)
  (with-accessors ((memory cpu-memory)) cpu
    (store memory operand (cpu-x cpu))))
