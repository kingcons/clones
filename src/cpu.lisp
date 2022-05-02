(mgl-pax:define-package :clones.cpu
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.opcodes :clones.memory :clones.util)
  (:import-from :clones.disassembler
                #:disassemble-instruction)
  (:import-from :serapeum
                #:octet))

(in-package :clones.cpu)

(defsection @cpu (:title "CPU Core")
  (cpu class)
  (make-cpu function)
  (cpu-memory (accessor cpu))
  (cpu-accum (accessor cpu))
  (cpu-x (accessor cpu))
  (cpu-y (accessor cpu))
  (cpu-status (accessor cpu))
  (cpu-stack (accessor cpu))
  (cpu-pc (accessor cpu))
  (cpu-cycles (accessor cpu))
  (single-step function)
  (now function)
  (nmi function))

(define-condition illegal-opcode (error)
  ((opcode :initarg :opcode :reader error-opcode))
  (:report (lambda (condition stream)
             (format stream "Clones does not support Illegal Opcode ~A"
                     (error-opcode condition)))))

(defclass cpu ()
  ((memory :initarg :memory :type memory
           :accessor cpu-memory)
   (accum :initform 0 :type octet
          :accessor cpu-accum)
   (x :initform 0 :type octet
      :accessor cpu-x)
   (y :initform 0 :type octet
      :accessor cpu-y)
   (status :initform #x24 :type octet
           :accessor cpu-status)
   (stack :initform #xFD :type octet
          :accessor cpu-stack)
   (pc :initform #xFFFC :type (unsigned-byte 16)
       :accessor cpu-pc)
   (cycles :initform 0 :type fixnum
           :accessor cpu-cycles)))

(defun make-cpu (&key (memory (make-memory)))
  (make-instance 'cpu :memory memory))

(defun now (cpu &key (stream t))
  "Disassemble the current instruction pointed to by the CPU's program counter.
   STREAM is the FORMAT destination for the disassembly."
  (disassemble-instruction (cpu-memory cpu) (cpu-pc cpu) :stream stream))

(defun get-address (cpu addressing-mode)
  (with-accessors ((memory cpu-memory)
                   (pc cpu-pc)) cpu
    (ecase addressing-mode
      (:implied nil)
      (:accumulator nil)
      (:immediate (1+ pc))
      (:zero-page (fetch memory (1+ pc)))
      (:zero-page-x (let ((start (fetch memory (1+ pc))))
                      (wrap-byte (+ start (cpu-x cpu)))))
      (:zero-page-y (let ((start (fetch memory (1+ pc))))
                      (wrap-byte (+ start (cpu-y cpu)))))
      (:absolute (fetch-word memory (1+ pc)))
      (:absolute-x (let* ((start (fetch-word memory (1+ pc)))
                          (destination (wrap-word (+ start (cpu-x cpu)))))
                     (values destination start)))
      (:absolute-y (let* ((start (fetch-word memory (1+ pc)))
                          (destination (wrap-word (+ start (cpu-y cpu)))))
                     (values destination start)))
      (:indirect (let ((start (fetch-word memory (1+ pc))))
                   (fetch-indirect memory start)))
      (:indirect-x (let* ((start (+ (fetch memory (1+ pc)) (cpu-x cpu))))
                     (fetch-indirect memory (wrap-byte start))))
      (:indirect-y (let* ((offset (fetch memory (1+ pc)))
                          (start (fetch-indirect memory offset))
                          (destination (wrap-word (+ start (cpu-y cpu)))))
                     (values destination start)))
      (:relative (let ((offset (fetch memory (+ pc 1)))
                       (next (+ pc 2)))   ; Instruction after the branch
                   (if (logbitp 7 offset) ; Branch backwards when negative
                       (- next (ldb (byte 7 0) offset))
                       (+ next offset)))))))

(defun get-operand (cpu opcode)
  (with-accessors ((memory cpu-memory)
                   (pc cpu-pc)) cpu
    (let ((mode (opcode-addressing-mode opcode))
          (access-pattern (opcode-access-pattern opcode)))
      (multiple-value-bind (destination start) (get-address cpu mode)
        (ecase access-pattern
          (:read (progn
                   (when (and start (page-crossed? start destination))
                     (incf (cpu-cycles cpu)))
                   (fetch memory destination)))
          (:write destination)
          (:jump destination)
          (:static nil)
          (:read-modify-write (if (eql mode :accumulator)
                                  (lambda (&optional new-value)
                                    (if new-value
                                        (setf (cpu-accum cpu) new-value)
                                        (cpu-accum cpu)))
                                  (lambda (&optional new-value)
                                    (if new-value
                                        (store (cpu-memory cpu) destination new-value)
                                        (fetch (cpu-memory cpu) destination))))))))))

(defun single-step (cpu)
  "Step the CPU over the current instruction."
  (with-accessors ((pc cpu-pc)
                   (memory cpu-memory)) cpu
    (let* ((opcode (find-opcode (fetch memory pc)))
           (handler (opcode-name opcode))
           (operand (get-operand cpu opcode)))
      (when (eql handler :illegal)
        (error 'illegal-opcode :opcode opcode))
      (funcall handler cpu operand)
      ;; Update the program counter and cycle count
      (unless (eql (opcode-access-pattern opcode) :jump)
        (incf pc (opcode-size opcode)))
      (incf (cpu-cycles cpu) (opcode-time opcode)))))

(defmacro branch-if (cpu condition destination)
  (with-gensyms (start pc)
    `(with-accessors ((,pc cpu-pc)) ,cpu
       (let ((,start (+ ,pc 2)))
         (if ,condition
             (progn
               (setf ,pc ,destination)
               (if (page-crossed? ,start ,destination)
                   (incf (cpu-cycles ,cpu) 2)
                   (incf (cpu-cycles ,cpu))))
             (incf ,pc 2))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %flag-index (flag)
    (let ((flags '(:carry :zero :interrupt :decimal
                   :break :unused :overflow :sign)))
      (position flag flags))))

(defmacro set-flag (cpu flag form)
  "Set the bit corresponding to FLAG in the status register of
   CPU to the result of the supplied FORM using (SETF LDB)."
  (let ((position (%flag-index flag)))
    `(setf (ldb (byte 1 ,position) (cpu-status ,cpu)) ,form)))

(defmacro status? (flag)
  "Check if a FLAG is enabled in the CPU's status register.
   Note that this macro is unhygienic and assumes a variable
   named CPU is bound in the lexical environment to a CPU struture."
  (let ((position (%flag-index flag)))
    `(logbitp ,position (cpu-status cpu))))

(defun overflow? (x y result)
  ;; Cribbing from 6502.txt version of ADC
  ;; (not (and (logbitp 7 (logxor x y))
  ;;            (logbitp 7 (logxor x result))))
  (flet ((sign-of (x) (logbitp 7 x)))
    (not (or (eql (sign-of result) (sign-of x))
             (eql (sign-of result) (sign-of y))))))

(defun page-crossed? (start end)
  (not (= (ldb (byte 8 8) start)
          (ldb (byte 8 8) end))))

(defun set-flag-zn (cpu operand)
  (set-flag cpu :zero (if (zerop operand) 1 0))
  (set-flag cpu :sign (if (logbitp 7 operand) 1 0)))

(defun stack-pop (cpu)
  (with-accessors ((memory cpu-memory)
                   (stack cpu-stack)) cpu
    (incf stack)
    (fetch memory (+ #x100 stack))))

(defun stack-push (cpu value)
  (with-accessors ((memory cpu-memory)
                   (stack cpu-stack)) cpu
    (store memory (+ #x100 stack) value)
    (decf stack)))

(defun stack-pop-word (cpu)
  (let ((low-byte (stack-pop cpu))
        (high-byte (stack-pop cpu)))
    (dpb high-byte (byte 8 8) low-byte)))

(defun stack-push-word (cpu address)
  (let ((low-byte (wrap-byte address))
        (high-byte (ldb (byte 8 8) address)))
    (stack-push cpu high-byte)
    (stack-push cpu low-byte)))

(defun nmi (cpu)
  (stack-push cpu (cpu-status cpu))
  (stack-push-word cpu (cpu-pc cpu))
  (setf (cpu-pc cpu) (fetch-word (cpu-memory cpu) #xFFFA)))

(defun :adc (cpu operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let* ((carry-bit (ldb (byte 1 0) (cpu-status cpu)))
           (result (+ accum operand carry-bit))
           (wrapped (logand result #xFF)))
      (set-flag cpu :overflow (if (overflow? accum operand result) 1 0))
      (set-flag cpu :carry (if (> result #xFF) 1 0))
      (set-flag-zn cpu wrapped)
      (setf accum wrapped))))

(defun :and (cpu operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let ((result (logand accum operand)))
      (setf accum result)
      (set-flag-zn cpu result))))

(defun :asl (cpu accessor)
  (let* ((operand (funcall accessor))
         (result (wrap-byte (ash operand 1))))
    (set-flag cpu :carry (ldb (byte 1 7) operand))
    (set-flag-zn cpu result)
    (funcall accessor result)))

(defun :bcc (cpu operand)
  (branch-if cpu (not (status? :carry)) operand))

(defun :bcs (cpu operand)
  (branch-if cpu (status? :carry) operand))

(defun :beq (cpu operand)
  (branch-if cpu (status? :zero) operand))

(defun :bit (cpu operand)
  (let ((result (logand (cpu-accum cpu) operand)))
    (set-flag cpu :zero (if (zerop result) 1 0))
    (set-flag cpu :overflow (if (logbitp 6 operand) 1 0))
    (set-flag cpu :sign (if (logbitp 7 operand) 1 0))))

(defun :bmi (cpu operand)
  (branch-if cpu (status? :sign) operand))

(defun :bne (cpu operand)
  (branch-if cpu (not (status? :zero)) operand))

(defun :bpl (cpu operand)
  (branch-if cpu (not (status? :sign)) operand))

(defun :brk (cpu operand)
  (stack-push-word cpu (1+ (cpu-pc cpu)))
  (set-flag cpu :break 1)
  (stack-push cpu (cpu-status cpu))
  (set-flag cpu :interrupt 1)
  (setf (cpu-pc cpu) (fetch-word (cpu-memory cpu) #xFFFE)))

(defun :bvc (cpu operand)
  (branch-if cpu (not (status? :overflow)) operand))

(defun :bvs (cpu operand)
  (branch-if cpu (status? :overflow) operand))

(defun :clc (cpu operand)
  (declare (ignore operand))
  (set-flag cpu :carry 0))

(defun :cld (cpu operand)
  (declare (ignore operand))
  (set-flag cpu :decimal 0))

(defun :cli (cpu operand)
  (declare (ignore operand))
  (set-flag cpu :interrupt 0))

(defun :clv (cpu operand)
  (declare (ignore operand))
  (set-flag cpu :overflow 0))

(defun :cmp (cpu operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let ((result (- accum operand)))
      (set-flag-zn cpu result)
      (set-flag cpu :carry (if (>= accum operand) 1 0)))))

(defun :cpx (cpu operand)
  (with-accessors ((x cpu-x)) cpu
    (let ((result (- x operand)))
      (set-flag-zn cpu result)
      (set-flag cpu :carry (if (>= x operand) 1 0)))))

(defun :cpy (cpu operand)
  (with-accessors ((y cpu-y)) cpu
    (let ((result (- y operand)))
      (set-flag-zn cpu result)
      (set-flag cpu :carry (if (>= y operand) 1 0)))))

(defun :dec (cpu address)
  (with-accessors ((memory cpu-memory)) cpu
    (let ((result (wrap-byte (1- (fetch memory address)))))
      (set-flag-zn cpu result)
      (store memory address result))))

(defun :dex (cpu operand)
  (declare (ignore operand))
  (with-accessors ((x cpu-x)) cpu
    (let ((result (logand (1- x) #xFF)))
      (setf x result)
      (set-flag-zn cpu result))))

(defun :dey (cpu operand)
  (declare (ignore operand))
  (with-accessors ((y cpu-y)) cpu
    (let ((result (logand (1- y) #xFF)))
      (setf y result)
      (set-flag-zn cpu result))))

(defun :eor (cpu operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let ((result (logxor accum operand)))
      (set-flag-zn cpu result)
      (setf accum result))))

(defun :inc (cpu address)
  (with-accessors ((memory cpu-memory)) cpu
    (let ((result (wrap-byte (1+ (fetch memory address)))))
      (set-flag-zn cpu result)
      (store memory address result))))

(defun :inx (cpu operand)
  (declare (ignore operand))
  (with-accessors ((x cpu-x)) cpu
    (let ((result (logand (1+ x) #xFF)))
      (setf x result)
      (set-flag-zn cpu result))))

(defun :iny (cpu operand)
  (declare (ignore operand))
  (with-accessors ((y cpu-y)) cpu
    (let ((result (logand (1+ y) #xFF)))
      (setf y result)
      (set-flag-zn cpu result))))

(defun :jmp (cpu operand)
  (setf (cpu-pc cpu) operand))

(defun :jsr (cpu operand)
  (let ((return-address (+ (cpu-pc cpu) 2)))
    (stack-push-word cpu return-address)
    (setf (cpu-pc cpu) operand)))

(defun :lda (cpu operand)
  (setf (cpu-accum cpu) operand)
  (set-flag-zn cpu operand))

(defun :ldx (cpu operand)
  (setf (cpu-x cpu) operand)
  (set-flag-zn cpu operand))

(defun :ldy (cpu operand)
  (setf (cpu-y cpu) operand)
  (set-flag-zn cpu operand))

(defun :lsr (cpu accessor)
  (let* ((operand (funcall accessor))
         (result (ash operand -1)))
    (set-flag cpu :carry (ldb (byte 1 0) operand))
    (set-flag-zn cpu result)
    (funcall accessor result)))

(defun :nop (cpu operand)
  (declare (ignore cpu operand)))

(defun :ora (cpu operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let ((result (logior accum operand)))
      (setf accum result)
      (set-flag-zn cpu result))))

(defun :pha (cpu operand)
  (declare (ignore operand))
  (stack-push cpu (cpu-accum cpu)))

(defun :php (cpu operand)
  (declare (ignore operand))
  (stack-push cpu (dpb 1 (byte 1 4) (cpu-status cpu))))

(defun :pla (cpu operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-accum cpu) (stack-pop cpu))))
    (set-flag-zn cpu result)))

(defun :plp (cpu operand)
  (declare (ignore operand))
  (setf (cpu-status cpu) (stack-pop cpu))
  (set-flag cpu :break 0)
  (set-flag cpu :unused 1))

(defun :rol (cpu accessor)
  (let* ((operand (funcall accessor))
         (carry-bit (ldb (byte 1 0) (cpu-status cpu)))
         (rotate-with-carry (dpb carry-bit (byte 1 0) (ash operand 1)))
         (result (wrap-byte rotate-with-carry)))
    (set-flag cpu :carry (ldb (byte 1 7) operand))
    (set-flag-zn cpu result)
    (funcall accessor result)))

(defun :ror (cpu accessor)
  (let* ((operand (funcall accessor))
         (carry-bit (ldb (byte 1 0) (cpu-status cpu)))
         (result (dpb carry-bit (byte 1 7) (ash operand -1))))
    (set-flag cpu :carry (ldb (byte 1 0) operand))
    (set-flag-zn cpu result)
    (funcall accessor result)))

(defun :rti (cpu operand)
  (declare (ignore operand))
  (let ((new-status (dpb 1 (byte 1 5) (stack-pop cpu)))
        (return-address (1- (stack-pop-word cpu))))
    (setf (cpu-status cpu) new-status
          (cpu-pc cpu) return-address)))

(defun :rts (cpu operand)
  (declare (ignore operand))
  (let ((return-address (stack-pop-word cpu)))
    (setf (cpu-pc cpu) return-address)))

(defun :sbc (cpu operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let* ((carry-bit (if (status? :carry) 0 1))
           (result (- accum operand carry-bit))
           (top-bit (ldb (byte 1 7) operand))
           (flipped (dpb (logxor top-bit 1) (byte 1 7) operand))
           (wrapped (logand result #xFF)))
      (set-flag cpu :overflow (if (overflow? accum flipped result) 1 0))
      (set-flag cpu :carry (if (not (minusp result)) 1 0))
      (set-flag-zn cpu wrapped)
      (setf accum wrapped))))

(defun :sec (cpu operand)
  (declare (ignore operand))
  (set-flag cpu :carry 1))

(defun :sed (cpu operand)
  (declare (ignore operand))
  (set-flag cpu :decimal 1))

(defun :sei (cpu operand)
  (declare (ignore operand))
  (set-flag cpu :interrupt 1))

(defun :sta (cpu operand)
  (with-accessors ((memory cpu-memory)) cpu
    (store memory operand (cpu-accum cpu))))

(defun :stx (cpu operand)
  (with-accessors ((memory cpu-memory)) cpu
    (store memory operand (cpu-x cpu))))

(defun :sty (cpu operand)
  (with-accessors ((memory cpu-memory)) cpu
    (store memory operand (cpu-y cpu))))

(defun :tax (cpu operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-x cpu) (cpu-accum cpu))))
    (set-flag-zn cpu result)))

(defun :tay (cpu operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-y cpu) (cpu-accum cpu))))
    (set-flag-zn cpu result)))

(defun :tsx (cpu operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-x cpu) (cpu-stack cpu))))
    (set-flag-zn cpu result)))

(defun :txa (cpu operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-accum cpu) (cpu-x cpu))))
    (set-flag-zn cpu result)))

(defun :txs (cpu operand)
  (declare (ignore operand))
  (setf (cpu-stack cpu) (cpu-x cpu)))

(defun :tya (cpu operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-accum cpu) (cpu-y cpu))))
    (set-flag-zn cpu result)))
