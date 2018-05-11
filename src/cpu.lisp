(in-package :cl-user)

(defpackage :clones.cpu
  (:use :cl)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :clones.memory
                :memory
                :make-memory
                :fetch
                :store)
  (:import-from :clones.util
                :ub8
                :ub16
                :wrap-byte)
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
           #:reset
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
           #:overflow-p))

(in-package :clones.cpu)

(defstruct cpu
  (memory (make-memory)  :type memory)
  (cycles 0              :type fixnum)
  (accum  0              :type ub8)
  (x-reg  0              :type ub8)
  (y-reg  0              :type ub8)
  (stack  #xfd           :type ub8)
  (status #x24           :type ub8)
  (pc     #xfffc         :type ub16))

(defun reset (cpu)
  "Jump to the address at the Reset Vector (0xFFFC)."
  (declare (type cpu cpu))
  (with-slots (memory pc) cpu
    (setf pc (clones.memory:fetch-word memory pc))))

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

(declaim (inline set-flags-zn))
(defun set-flags-zn (cpu value)
  (declare (type cpu cpu)
           (type fixnum value))
  (set-flag-if cpu :zero (zerop value))
  (set-flag-if cpu :negative (logbitp 7 value)))

(defun compare (cpu register memory)
  (declare (type cpu cpu)
           (type ub8 register memory))
  (let ((result (- register memory)))
    (set-flag-if cpu :carry (>= result 0))
    (set-flags-zn cpu result)))

(declaim (ftype (function (cpu ub8) ub8) stack-push))
(defun stack-push (cpu value)
  (declare (type cpu cpu))
  (store (cpu-memory cpu) (+ (cpu-stack cpu) #x100) value)
  (setf (cpu-stack cpu) (wrap-byte (1- (cpu-stack cpu)))))

(defun stack-push-word (cpu value)
  (declare (type ub16 value))
  (stack-push cpu (ash value -8))
  (stack-push cpu (wrap-byte value)))

(declaim (ftype (function (cpu) ub8) stack-pop))
(defun stack-pop (cpu)
  (declare (type cpu cpu))
  (setf (cpu-stack cpu) (wrap-byte (1+ (cpu-stack cpu))))
  (fetch (cpu-memory cpu) (+ (cpu-stack cpu) #x100)))

(declaim (ftype (function (cpu) ub16) stack-pop-word))
(defun stack-pop-word (cpu)
  (let ((low-byte (stack-pop cpu))
        (high-byte (stack-pop cpu)))
    (declare (type ub8 low-byte high-byte))
    (+ low-byte (ash high-byte 8))))

(declaim (ftype (function (fixnum ub8 ub8) boolean) overflow-p))
(defun overflow-p (result augend addend)
  (flet ((sign-bit (x) (logbitp 7 x)))
    (let ((result-sign (sign-bit result)))
      (not (or (eql result-sign (sign-bit augend))
               (eql result-sign (sign-bit addend)))))))
