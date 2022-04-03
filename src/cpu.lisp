(mgl-pax:define-package :clones.cpu
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.opcodes :clones.memory)
  (:import-from :clones.disassembler #:disassemble-instruction))

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

(defun now (cpu &key (stream t))
  "Disassemble the current instruction pointed to by the CPU's program counter.
   STREAM is the FORMAT destination for the disassembly."
  (disassemble-instruction (cpu-memory cpu) (cpu-pc cpu) :stream stream))

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
                            (dpb high-byte (byte 8 8) low-byte)))
               (:relative (let ((offset (fetch memory (+ pc 1)))
                                (next (+ pc 2))) ; Instruction after the branch
                            (if (logbitp 7 offset) ; Branch backwards when negative
                                (- next (ldb (byte 7 0) offset))
                                (+ next offset))))
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
    (let* ((opcode (find-opcode (fetch memory pc)))
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

(defun page-crossed? (start end)
  (not (= (ldb (byte 8 8) start)
          (ldb (byte 8 8) end))))

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
      (dpb high-byte (byte 8 8) low-byte))))

(defun stack-push-address (cpu address)
  (with-accessors ((memory cpu-memory)
                   (stack cpu-stack)) cpu
    (let ((low-byte (ldb (byte 8 0) address))
          (high-byte (ldb (byte 8 8) address)))
      (store memory stack high-byte)
      (decf stack)
      (store memory stack low-byte)
      (decf stack))))

(defun :and (cpu operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let ((result (logand accum operand)))
      (setf accum result)
      (set-flag-zn cpu result))))

(defun :bcc (cpu operand)
  (branch-if cpu (not (logbitp 0 (cpu-status cpu))) operand))

(defun :bcs (cpu operand)
  (branch-if cpu (logbitp 0 (cpu-status cpu)) operand))

(defun :beq (cpu operand)
  (branch-if cpu (logbitp 1 (cpu-status cpu)) operand))

(defun :bit (cpu operand)
  (let ((result (logand (cpu-accum cpu) operand)))
    (with-accessors ((status cpu-status)) cpu
      (setf status (deposit-field operand (byte 2 6) status))
      (setf status (dpb (if (zerop result) 1 0) (byte 1 1) status)))))

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

(defun :php (cpu operand)
  (declare (ignore operand))
  (with-accessors ((stack cpu-stack)) cpu
    (store (cpu-memory cpu) stack (cpu-status cpu))
    (decf stack)))

(defun :pla (cpu operand)
  (declare (ignore operand))
  (with-accessors ((stack cpu-stack)) cpu
    (let ((result (fetch (cpu-memory cpu) (1+ stack))))
      (setf (cpu-accum cpu) (logior result #b0010000))
      (set-flag-zn cpu result))
    (incf stack)))

(defun :rts (cpu operand)
  (declare (ignore operand))
  (let ((return-address (stack-pop-address cpu)))
    (setf (cpu-pc cpu) return-address)))

(defun :sec (cpu operand)
  (declare (ignore operand))
  (with-accessors ((status cpu-status)) cpu
    (setf status (logior status #b00000001))))

(defun :sed (cpu operand)
  (declare (ignore operand))
  (with-accessors ((status cpu-status)) cpu
    (setf status (logior status #b00001000))))

(defun :sei (cpu operand)
  (declare (ignore operand))
  (with-accessors ((status cpu-status)) cpu
    (setf status (logior status #b00000100))))

(defun :sta (cpu operand)
  (with-accessors ((memory cpu-memory)) cpu
    (store memory operand (cpu-accum cpu))))

(defun :stx (cpu operand)
  (with-accessors ((memory cpu-memory)) cpu
    (store memory operand (cpu-x cpu))))
