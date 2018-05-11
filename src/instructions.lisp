(in-package :cl-user)

(defpackage :clones.instructions
  (:use :cl :clones.addressing :clones.cpu)
  (:import-from :clones.memory
                :fetch
                :store)
  (:import-from :clones.util
                :*standard-optimize-settings*
                :wrap-byte)
  (:import-from :clones.instruction-data
                :*instructions*
                :%build-op-name
                :get-instruction-meta
                :jump-table)
  (:export #:single-step))

(in-package :clones.instructions)

(defmacro define-instruction (name () &body body)
  (let ((metadata (get-instruction-meta name)))
    (destructuring-bind (name opcodes docs &optional skip-pc) metadata
      (declare (ignore docs))
      `(progn
         ,@(loop for (opcode bytes cycles mode raw) in opcodes
                 ;; KLUDGE: Find the symbol since it doesn't exist at instruction-data load-time.
                 for addr-mode = (find-symbol (symbol-name mode))
                 collect `(%define-opcode (,name ,opcode ,addr-mode
                                           :bytes ,bytes :cycles ,cycles
                                           :raw ,raw :skip-pc ,skip-pc)
                            ,@body))
         ,@(loop for (opcode) in opcodes
                 collect `(export ',(%build-op-name name opcode) 'clones.instructions))))))

(defmacro %define-opcode ((name opcode address-mode &key bytes cycles raw skip-pc)
                         &body body)
  `(defun ,(%build-op-name name opcode) (cpu)
     (declare (type cpu cpu))
     (declare #.*standard-optimize-settings*)
     (incf (cpu-pc cpu))
     ,(cond ((null address-mode) `(progn ,@body))
            (raw `(let ((argument (,address-mode cpu)))
                    ,@body))
            (t `(let ((argument (fetch (cpu-memory cpu) (,address-mode cpu))))
                  ,@body)))
     (incf (cpu-cycles cpu) ,cycles)
     ,@(unless (or skip-pc (= 1 bytes))
         `((incf (cpu-pc cpu) ,(1- bytes))))))

(defmacro branch-if (test)
  `(if ,test
       (setf (cpu-pc cpu) argument)
       (incf (cpu-pc cpu))))

(define-instruction bcs ()
  (branch-if (flag-set-p cpu :carry)))

(define-instruction clc ()
  (set-flag cpu :carry 0))

(define-instruction inx ()
  (let ((result (wrap-byte (1+ (cpu-x-reg cpu)))))
    (setf (cpu-x-reg cpu) result)
    (set-flags-zn cpu result)))

(define-instruction jmp ()
  (setf (cpu-pc cpu) argument))

(define-instruction jsr ()
  (stack-push-word cpu (1+ (cpu-pc cpu)))
  (setf (cpu-pc cpu) argument))

(define-instruction ldx ()
  (let ((result (setf (cpu-x-reg cpu) argument)))
    (set-flags-zn cpu result)))

(define-instruction nop ()
  nil)

(define-instruction sec ()
  (set-flag cpu :carry 1))

(define-instruction sei ()
  (set-flag cpu :interrupt 1))

(define-instruction stx ()
  (store (cpu-memory cpu) argument (cpu-x-reg cpu)))

(defun single-step (cpu)
  "Execute a single instruction and return the CPU."
  (declare (type cpu cpu))
  (jump-table (fetch (cpu-memory cpu) (cpu-pc cpu))))
