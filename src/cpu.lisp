(mgl-pax:define-package :clones.cpu
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.opcodes :clones.memory)
  (:import-from :clones.disassembler
                #:disassemble-instruction)
  (:import-from :clones.util
                #:define-printer
                #:wrap-byte
                #:wrap-word)
  (:import-from :serapeum
                #:octet))

(in-package :clones.cpu)

(defsection @cpu (:title "CPU Core")
  (cpu class)
  (make-cpu function)
  (cpu-memory (accessor cpu))
  (cpu-cycles (accessor cpu))
  (single-step function)
  (reset function)
  (now function)
  (nmi function)
  (change-game function))

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

(define-printer cpu (accum x y status stack pc cycles)
  "Acc: ~2,'0X X: ~2,'0X Y: ~2,'0X Status: ~2,'0X Stack: ~2,'0X PC: ~4,'0X Cycles: ~D")

(defun make-cpu (&key (memory (make-memory)))
  (make-instance 'cpu :memory memory))

(defun change-game (cpu relative-path)
  (swap-cart (cpu-memory cpu) relative-path)
  (reset cpu))

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
      (:relative (let* ((next-instruction (+ pc 2))
                        (offset (fetch memory (1+ pc))))
                   (if (logbitp 7 offset) ; Branch backwards when negative
                       (+ next-instruction (logior offset -128))
                       (+ next-instruction offset)))))))

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
           (instruction (opcode-name opcode))
           (operand (get-operand cpu opcode)))
      (execute cpu instruction operand)
      (unless (eql (opcode-access-pattern opcode) :jump)
        (incf pc (opcode-size opcode)))
      (incf (cpu-cycles cpu) (opcode-time opcode)))))

(defmacro branch-if (cpu condition destination)
  (with-gensyms (next pc)
    `(with-accessors ((,pc cpu-pc)) ,cpu
       (let ((,next (+ ,pc 2)))
         (if ,condition
             (progn
               (setf ,pc ,destination)
               (if (page-crossed? ,next ,destination)
                   (incf (cpu-cycles ,cpu) 2)
                   (incf (cpu-cycles ,cpu))))
             (setf ,pc ,next))))))

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
  (stack-push-word cpu (cpu-pc cpu))
  (stack-push cpu (cpu-status cpu))
  (setf (cpu-pc cpu) (fetch-word (cpu-memory cpu) #xFFFA)))

(defun reset (cpu)
  (setf (cpu-pc cpu) (fetch-word (cpu-memory cpu) #xFFFC)))

(defgeneric execute (cpu instruction operand)
  (:documentation "Execute the given INSTRUCTION with its OPERAND on the CPU.")
  (:method ((cpu cpu) instruction operand)
    (error 'illegal-opcode :opcode (find-opcode (fetch (cpu-memory cpu) (cpu-pc cpu))))))

(defmethod execute ((cpu cpu) (instruction (eql :adc)) operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let* ((carry-bit (ldb (byte 1 0) (cpu-status cpu)))
           (result (+ accum operand carry-bit))
           (wrapped (logand result #xFF)))
      (set-flag cpu :overflow (if (overflow? accum operand result) 1 0))
      (set-flag cpu :carry (if (> result #xFF) 1 0))
      (set-flag-zn cpu wrapped)
      (setf accum wrapped))))

(defmethod execute ((cpu cpu) (instruction (eql :and)) operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let ((result (logand accum operand)))
      (setf accum result)
      (set-flag-zn cpu result))))

(defmethod execute ((cpu cpu) (instruction (eql :asl)) accessor)
  (let* ((operand (funcall accessor))
         (result (wrap-byte (ash operand 1))))
    (set-flag cpu :carry (ldb (byte 1 7) operand))
    (set-flag-zn cpu result)
    (funcall accessor result)))

(defmethod execute ((cpu cpu) (instruction (eql :bcc)) operand)
  (branch-if cpu (not (status? :carry)) operand))

(defmethod execute ((cpu cpu) (instruction (eql :bcs)) operand)
  (branch-if cpu (status? :carry) operand))

(defmethod execute ((cpu cpu) (instruction (eql :beq)) operand)
  (branch-if cpu (status? :zero) operand))

(defmethod execute ((cpu cpu) (instruction (eql :bit)) operand)
  (let ((result (logand (cpu-accum cpu) operand)))
    (set-flag cpu :zero (if (zerop result) 1 0))
    (set-flag cpu :overflow (if (logbitp 6 operand) 1 0))
    (set-flag cpu :sign (if (logbitp 7 operand) 1 0))))

(defmethod execute ((cpu cpu) (instruction (eql :bmi)) operand)
  (branch-if cpu (status? :sign) operand))

(defmethod execute ((cpu cpu) (instruction (eql :bne)) operand)
  (branch-if cpu (not (status? :zero)) operand))

(defmethod execute ((cpu cpu) (instruction (eql :bpl)) operand)
  (branch-if cpu (not (status? :sign)) operand))

(defmethod execute ((cpu cpu) (instruction (eql :brk)) operand)
  (stack-push-word cpu (1+ (cpu-pc cpu)))
  (set-flag cpu :break 1)
  (stack-push cpu (cpu-status cpu))
  (set-flag cpu :interrupt 1)
  (setf (cpu-pc cpu) (fetch-word (cpu-memory cpu) #xFFFE)))

(defmethod execute ((cpu cpu) (instruction (eql :bvc)) operand)
  (branch-if cpu (not (status? :overflow)) operand))

(defmethod execute ((cpu cpu) (instruction (eql :bvs)) operand)
  (branch-if cpu (status? :overflow) operand))

(defmethod execute ((cpu cpu) (instruction (eql :clc)) operand)
  (declare (ignore operand))
  (set-flag cpu :carry 0))

(defmethod execute ((cpu cpu) (instruction (eql :cld)) operand)
  (declare (ignore operand))
  (set-flag cpu :decimal 0))

(defmethod execute ((cpu cpu) (instruction (eql :cli)) operand)
  (declare (ignore operand))
  (set-flag cpu :interrupt 0))

(defmethod execute ((cpu cpu) (instruction (eql :clv)) operand)
  (declare (ignore operand))
  (set-flag cpu :overflow 0))

(defmethod execute ((cpu cpu) (instruction (eql :cmp)) operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let ((result (- accum operand)))
      (set-flag-zn cpu result)
      (set-flag cpu :carry (if (>= accum operand) 1 0)))))

(defmethod execute ((cpu cpu) (instruction (eql :cpx)) operand)
  (with-accessors ((x cpu-x)) cpu
    (let ((result (- x operand)))
      (set-flag-zn cpu result)
      (set-flag cpu :carry (if (>= x operand) 1 0)))))

(defmethod execute ((cpu cpu) (instruction (eql :cpy)) operand)
  (with-accessors ((y cpu-y)) cpu
    (let ((result (- y operand)))
      (set-flag-zn cpu result)
      (set-flag cpu :carry (if (>= y operand) 1 0)))))

(defmethod execute ((cpu cpu) (instruction (eql :dec)) operand)
  (with-accessors ((memory cpu-memory)) cpu
    (let ((result (wrap-byte (1- (fetch memory operand)))))
      (set-flag-zn cpu result)
      (store memory operand result))))

(defmethod execute ((cpu cpu) (instruction (eql :dex)) operand)
  (declare (ignore operand))
  (with-accessors ((x cpu-x)) cpu
    (let ((result (logand (1- x) #xFF)))
      (setf x result)
      (set-flag-zn cpu result))))

(defmethod execute ((cpu cpu) (instruction (eql :dey)) operand)
  (declare (ignore operand))
  (with-accessors ((y cpu-y)) cpu
    (let ((result (logand (1- y) #xFF)))
      (setf y result)
      (set-flag-zn cpu result))))

(defmethod execute ((cpu cpu) (instruction (eql :eor)) operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let ((result (logxor accum operand)))
      (set-flag-zn cpu result)
      (setf accum result))))

(defmethod execute ((cpu cpu) (instruction (eql :inc)) operand)
  (with-accessors ((memory cpu-memory)) cpu
    (let ((result (wrap-byte (1+ (fetch memory operand)))))
      (set-flag-zn cpu result)
      (store memory operand result))))

(defmethod execute ((cpu cpu) (instruction (eql :inx)) operand)
  (declare (ignore operand))
  (with-accessors ((x cpu-x)) cpu
    (let ((result (logand (1+ x) #xFF)))
      (setf x result)
      (set-flag-zn cpu result))))

(defmethod execute ((cpu cpu) (instruction (eql :iny)) operand)
  (declare (ignore operand))
  (with-accessors ((y cpu-y)) cpu
    (let ((result (logand (1+ y) #xFF)))
      (setf y result)
      (set-flag-zn cpu result))))

(defmethod execute ((cpu cpu) (instruction (eql :jmp)) operand)
  (setf (cpu-pc cpu) operand))

(defmethod execute ((cpu cpu) (instruction (eql :jsr)) operand)
  (let ((return-address (+ (cpu-pc cpu) 2)))
    (stack-push-word cpu return-address)
    (setf (cpu-pc cpu) operand)))

(defmethod execute ((cpu cpu) (instruction (eql :lda)) operand)
  (setf (cpu-accum cpu) operand)
  (set-flag-zn cpu operand))

(defmethod execute ((cpu cpu) (instruction (eql :ldx)) operand)
  (setf (cpu-x cpu) operand)
  (set-flag-zn cpu operand))

(defmethod execute ((cpu cpu) (instruction (eql :ldy)) operand)
  (setf (cpu-y cpu) operand)
  (set-flag-zn cpu operand))

(defmethod execute ((cpu cpu) (instruction (eql :lsr)) accessor)
  (let* ((operand (funcall accessor))
         (result (ash operand -1)))
    (set-flag cpu :carry (ldb (byte 1 0) operand))
    (set-flag-zn cpu result)
    (funcall accessor result)))

(defmethod execute ((cpu cpu) (instruction (eql :nop)) operand)
  (declare (ignore cpu operand)))

(defmethod execute ((cpu cpu) (instruction (eql :ora)) operand)
  (with-accessors ((accum cpu-accum)) cpu
    (let ((result (logior accum operand)))
      (setf accum result)
      (set-flag-zn cpu result))))

(defmethod execute ((cpu cpu) (instruction (eql :pha)) operand)
  (declare (ignore operand))
  (stack-push cpu (cpu-accum cpu)))

(defmethod execute ((cpu cpu) (instruction (eql :php)) operand)
  (declare (ignore operand))
  (stack-push cpu (dpb 1 (byte 1 4) (cpu-status cpu))))

(defmethod execute ((cpu cpu) (instruction (eql :pla)) operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-accum cpu) (stack-pop cpu))))
    (set-flag-zn cpu result)))

(defmethod execute ((cpu cpu) (instruction (eql :plp)) operand)
  (declare (ignore operand))
  (setf (cpu-status cpu) (stack-pop cpu))
  (set-flag cpu :break 0)
  (set-flag cpu :unused 1))

(defmethod execute ((cpu cpu) (instruction (eql :rol)) accessor)
  (let* ((operand (funcall accessor))
         (carry-bit (ldb (byte 1 0) (cpu-status cpu)))
         (rotate-with-carry (dpb carry-bit (byte 1 0) (ash operand 1)))
         (result (wrap-byte rotate-with-carry)))
    (set-flag cpu :carry (ldb (byte 1 7) operand))
    (set-flag-zn cpu result)
    (funcall accessor result)))

(defmethod execute ((cpu cpu) (instruction (eql :ror)) accessor)
  (let* ((operand (funcall accessor))
         (carry-bit (ldb (byte 1 0) (cpu-status cpu)))
         (result (dpb carry-bit (byte 1 7) (ash operand -1))))
    (set-flag cpu :carry (ldb (byte 1 0) operand))
    (set-flag-zn cpu result)
    (funcall accessor result)))

(defmethod execute ((cpu cpu) (instruction (eql :rti)) operand)
  (declare (ignore operand))
  (let ((new-status (dpb 1 (byte 1 5) (stack-pop cpu)))
        (return-address (1- (stack-pop-word cpu))))
    (setf (cpu-status cpu) new-status
          (cpu-pc cpu) return-address)))

(defmethod execute ((cpu cpu) (instruction (eql :rts)) operand)
  (declare (ignore operand))
  (let ((return-address (stack-pop-word cpu)))
    (setf (cpu-pc cpu) return-address)))

(defmethod execute ((cpu cpu) (instruction (eql :sbc)) operand)
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

(defmethod execute ((cpu cpu) (instruction (eql :sec)) operand)
  (declare (ignore operand))
  (set-flag cpu :carry 1))

(defmethod execute ((cpu cpu) (instruction (eql :sed)) operand)
  (declare (ignore operand))
  (set-flag cpu :decimal 1))

(defmethod execute ((cpu cpu) (instruction (eql :sei)) operand)
  (declare (ignore operand))
  (set-flag cpu :interrupt 1))

(defmethod execute ((cpu cpu) (instruction (eql :sta)) operand)
  (with-accessors ((memory cpu-memory)) cpu
    (store memory operand (cpu-accum cpu))))

(defmethod execute ((cpu cpu) (instruction (eql :stx)) operand)
  (with-accessors ((memory cpu-memory)) cpu
    (store memory operand (cpu-x cpu))))

(defmethod execute ((cpu cpu) (instruction (eql :sty)) operand)
  (with-accessors ((memory cpu-memory)) cpu
    (store memory operand (cpu-y cpu))))

(defmethod execute ((cpu cpu) (instruction (eql :tax)) operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-x cpu) (cpu-accum cpu))))
    (set-flag-zn cpu result)))

(defmethod execute ((cpu cpu) (instruction (eql :tay)) operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-y cpu) (cpu-accum cpu))))
    (set-flag-zn cpu result)))

(defmethod execute ((cpu cpu) (instruction (eql :tsx)) operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-x cpu) (cpu-stack cpu))))
    (set-flag-zn cpu result)))

(defmethod execute ((cpu cpu) (instruction (eql :txa)) operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-accum cpu) (cpu-x cpu))))
    (set-flag-zn cpu result)))

(defmethod execute ((cpu cpu) (instruction (eql :txs)) operand)
  (declare (ignore operand))
  (setf (cpu-stack cpu) (cpu-x cpu)))

(defmethod execute ((cpu cpu) (instruction (eql :tya)) operand)
  (declare (ignore operand))
  (let ((result (setf (cpu-accum cpu) (cpu-y cpu))))
    (set-flag-zn cpu result)))
