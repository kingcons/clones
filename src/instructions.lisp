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
       (progn
         (incf (cpu-cycles cpu))
         (setf (cpu-pc cpu) argument))
       (incf (cpu-pc cpu))))

(define-instruction adc ()
  (let ((result (+ (cpu-accum cpu) argument (status-bit cpu :carry))))
    (set-flag-if cpu :carry (> result #xff))
    (set-flag-if cpu :overflow (overflow-p result (cpu-accum cpu) argument))
    (let ((wrapped (wrap-byte result)))
      (set-flags-zn cpu wrapped)
      (setf (cpu-accum cpu) wrapped))))

(define-instruction and ()
  (let ((result (setf (cpu-accum cpu) (logand (cpu-accum cpu) argument))))
    (set-flags-zn cpu result)))

(define-instruction bcc ()
  (branch-if (not (flag-set-p cpu :carry))))

(define-instruction bcs ()
  (branch-if (flag-set-p cpu :carry)))

(define-instruction beq ()
  (branch-if (flag-set-p cpu :zero)))

(define-instruction bit ()
  (set-flag-if cpu :zero (zerop (logand (cpu-accum cpu) argument)))
  (set-flag-if cpu :negative (logbitp 7 argument))
  (set-flag-if cpu :overflow (logbitp 6 argument)))

(define-instruction bmi ()
  (branch-if (flag-set-p cpu :negative)))

(define-instruction bne ()
  (branch-if (not (flag-set-p cpu :zero))))

(define-instruction bpl ()
  (branch-if (not (flag-set-p cpu :negative))))

(define-instruction bvc ()
  (branch-if (not (flag-set-p cpu :overflow))))

(define-instruction bvs ()
  (branch-if (flag-set-p cpu :overflow)))

(define-instruction clc ()
  (set-flag cpu :carry 0))

(define-instruction cld ()
  (set-flag cpu :decimal 0))

(define-instruction clv ()
  (set-flag cpu :overflow 0))

(define-instruction cmp ()
  (compare cpu (cpu-accum cpu) argument))

(define-instruction cpx ()
  (compare cpu (cpu-x-reg cpu) argument))

(define-instruction cpy ()
  (compare cpu (cpu-y-reg cpu) argument))

(define-instruction eor ()
  (let ((result (setf (cpu-accum cpu) (logxor (cpu-accum cpu) argument))))
    (set-flags-zn cpu result)))

(define-instruction inx ()
  (let ((result (wrap-byte (1+ (cpu-x-reg cpu)))))
    (setf (cpu-x-reg cpu) result)
    (set-flags-zn cpu result)))

(define-instruction iny ()
  (let ((result (wrap-byte (1+ (cpu-y-reg cpu)))))
    (setf (cpu-y-reg cpu) result)
    (set-flags-zn cpu result)))

(define-instruction jmp ()
  (setf (cpu-pc cpu) argument))

(define-instruction jsr ()
  (stack-push-word cpu (1+ (cpu-pc cpu)))
  (setf (cpu-pc cpu) argument))

(define-instruction lda ()
  (let ((result (setf (cpu-accum cpu) argument)))
    (set-flags-zn cpu result)))

(define-instruction ldx ()
  (let ((result (setf (cpu-x-reg cpu) argument)))
    (set-flags-zn cpu result)))

(define-instruction ldy ()
  (let ((result (setf (cpu-y-reg cpu) argument)))
    (set-flags-zn cpu result)))

(define-instruction nop ()
  nil)

(define-instruction ora ()
  (let ((result (setf (cpu-accum cpu) (logior (cpu-accum cpu) argument))))
    (set-flags-zn cpu result)))

(define-instruction pha ()
  (stack-push cpu (cpu-accum cpu)))

(define-instruction php ()
  (stack-push cpu (logior (cpu-status cpu) #x10)))

(define-instruction pla ()
  (let ((result (setf (cpu-accum cpu) (stack-pop cpu))))
    (set-flags-zn cpu result)))

(define-instruction plp ()
  (setf (cpu-status cpu) (logandc2 (logior (stack-pop cpu) #x20) #x10)))

(define-instruction rts ()
  (setf (cpu-pc cpu) (1+ (stack-pop-word cpu))))

(define-instruction sbc ()
  (let* ((carry-bit (if (flag-set-p cpu :carry) 0 1))
         (result (- (cpu-accum cpu) argument carry-bit)))
    (set-flag-if cpu :overflow (overflow-p result (cpu-accum cpu) argument))
    (set-flag-if cpu :carry (>= result 0))
    (let ((wrapped (wrap-byte result)))
      (set-flags-zn cpu wrapped)
      (setf (cpu-accum cpu) wrapped))))

(define-instruction sec ()
  (set-flag cpu :carry 1))

(define-instruction sed ()
  (set-flag cpu :decimal 1))

(define-instruction sei ()
  (set-flag cpu :interrupt 1))

(define-instruction sta ()
  (store (cpu-memory cpu) argument (cpu-accum cpu)))

(define-instruction stx ()
  (store (cpu-memory cpu) argument (cpu-x-reg cpu)))

(defun single-step (cpu)
  "Execute a single instruction and return the CPU."
  (declare (type cpu cpu))
  (jump-table (fetch (cpu-memory cpu) (cpu-pc cpu))))
