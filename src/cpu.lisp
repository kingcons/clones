(in-package :cl-user)

(defpackage :clones.cpu
  (:use :cl)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :clones.memory
                :memory
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
           #:update-flag
           #:set-flags-zn
           #:compare
           #:stack-push
           #:stack-push-word
           #:stack-pop
           #:stack-pop-word
           #:branch-if))

(in-package :clones.cpu)

(defstruct cpu
  (memory nil    :type memory)
  (cycles 0      :type fixnum)
  (accum  0      :type ub8)
  (x-reg  0      :type ub8)
  (y-reg  0      :type ub8)
  (stack  #xfd   :type ub8)
  (status #x24   :type ub8)
  (pc     #xfffc :type ub16))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %flag-index (flag)
    (let ((flags '(:carry :zero :interrupt :decimal
                   :break :unused :overflow :negative)))
      (position flag flags :test #'eq))))

(defmacro flag-set-p (cpu flag)
  `(logbitp ,(%flag-index flag) ,(cpu-status cpu)))

(defmacro update-flag (cpu flag test)
  (let ((index (%flag-index flag)))
    (with-gensyms (new-bit)
      `(let ((,new-bit (if ,test 1 0)))
         (setf (ldb (byte 1 ,index) (cpu-status ,cpu)) ,new-bit)))))

(declaim (inline set-flags-zn))
(defun set-flags-zn (cpu value)
  (declare (type cpu cpu)
           (type ub8 value))
  (update-flag cpu :zero (zerop value))
  (update-flag cpu :negative (logbitp 7 value)))

(defun compare (cpu register memory)
  (declare (type cpu cpu)
           (type ub8 register memory))
  (let ((result (- register memory)))
    (update-flag cpu :carry (>= result 0))
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

(defun stack-pop-word (cpu)
  (let ((low-byte (stack-pop cpu))
        (high-byte (stack-pop cpu)))
    (declare (type ub8 low-byte high-byte))
    (+ low-byte (ash high-byte 8))))

(defmacro branch-if (test)
  `(if ,test
       (setf (cpu-pc cpu) (relative cpu))
       (incf (cpu-pc cpu))))
