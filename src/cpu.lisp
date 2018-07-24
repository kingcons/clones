(in-package :cl-user)

(defpackage :clones.cpu
  (:use :cl)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :clones.memory
                :memory
                :memory-ppu
                :make-memory
                :fetch
                :store)
  (:import-from :clones.ppu
                :ppu-result)
  (:import-from :clones.util
                :ub8
                :ub16
                :ub32
                :wrap-byte
                :page-crossed-p)
  (:export #:cpu
           #:make-cpu
           #:cpu-memory
           #:cpu-cycles
           #:cpu-accum
           #:cpu-x-reg
           #:cpu-y-reg
           #:cpu-stack
           #:cpu-status
           #:cpu-pc
           #:cpu-waiting
           #:reset
           #:nmi
           #:irq
           #:dma
           #:status-bit
           #:flag-set-p
           #:set-flag
           #:set-flag-if
           #:set-flags-zn
           #:compare
           #:stack-push
           #:stack-push-word
           #:stack-pop
           #:stack-pop-word
           #:overflow-p
           #:maybe-update-cycle-count))

(in-package :clones.cpu)

(defstruct cpu
  (memory  (make-memory)  :type memory)
  (waiting 0              :type ub32)
  (cycles  0              :type ub32)
  (accum   0              :type ub8)
  (x-reg   0              :type ub8)
  (y-reg   0              :type ub8)
  (stack   #xfd           :type ub8)
  (status  #x24           :type ub8)
  (pc      #xfffc         :type ub16))

(defmethod print-object ((cpu cpu) stream)
  (print-unreadable-object (cpu stream)
    (with-slots (pc accum x-reg y-reg stack status cycles) cpu
      (format stream "CPU PC:~4,'0x Stack:~2,'0x Cycles:~D " pc stack cycles)
      (format stream "Acc:~2,'0x X:~2,'0x Y:~2,'0x Status:~8,'0b" accum x-reg y-reg status))))

(defun reset (cpu)
  "Jump to the address at the Reset Vector (0xFFFC)."
  (with-accessors ((memory cpu-memory) (pc cpu-pc)) cpu
    (setf pc (clones.memory:fetch-word memory #xFFFC))))

(defun interrupt-goto (cpu vector)
  (with-accessors ((memory cpu-memory) (pc cpu-pc) (status cpu-status)) cpu
    (stack-push-word cpu pc)
    (stack-push cpu status)
    (setf pc (clones.memory:fetch-word memory vector))))

(defun nmi (cpu)
  (interrupt-goto cpu #xFFFA))

(defun irq (cpu)
  (when (flag-set-p cpu :interrupt)
    (interrupt-goto cpu #xFFFE)))

(defun dma (cpu)
  (setf (cpu-waiting cpu) 512)
  (setf (getf (ppu-result (memory-ppu (cpu-memory cpu))) :dma) nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %flag-index (flag)
    (let ((flags '(:carry :zero :interrupt :decimal
                   :break :unused :overflow :negative)))
      (position flag flags :test #'eq))))

(defmacro status-bit (cpu flag)
  `(ldb (byte 1 ,(%flag-index flag)) (cpu-status ,cpu)))

(defmacro flag-set-p (cpu flag)
  `(logbitp ,(%flag-index flag) (cpu-status ,cpu)))

(defmacro set-flag (cpu flag value)
  `(setf (ldb (byte 1 ,(%flag-index flag)) (cpu-status ,cpu)) ,value))

(defmacro set-flag-if (cpu flag test)
  `(set-flag ,cpu ,flag (if ,test 1 0)))

(defun set-flags-zn (cpu value)
  (set-flag-if cpu :zero (zerop value))
  (set-flag-if cpu :negative (logbitp 7 value)))

(defun compare (cpu register memory)
  (let ((result (- register memory)))
    (set-flag-if cpu :carry (>= result 0))
    (set-flags-zn cpu result)))

(defun stack-push (cpu value)
  (store (cpu-memory cpu) (+ (cpu-stack cpu) #x100) value)
  (setf (cpu-stack cpu) (wrap-byte (1- (cpu-stack cpu)))))

(defun stack-push-word (cpu value)
  (stack-push cpu (ash value -8))
  (stack-push cpu (wrap-byte value)))

(defun stack-pop (cpu)
  (setf (cpu-stack cpu) (wrap-byte (1+ (cpu-stack cpu))))
  (fetch (cpu-memory cpu) (+ (cpu-stack cpu) #x100)))

(defun stack-pop-word (cpu)
  (let ((low-byte (stack-pop cpu))
        (high-byte (stack-pop cpu)))
    (+ low-byte (ash high-byte 8))))

(defun overflow-p (result augend addend)
  (flet ((sign-bit (x) (logbitp 7 x)))
    (let ((result-sign (sign-bit result)))
      (not (or (eql result-sign (sign-bit augend))
               (eql result-sign (sign-bit addend)))))))

(defun maybe-update-cycle-count (cpu start final)
  (when (page-crossed-p start final)
    (incf (cpu-cycles cpu))))
