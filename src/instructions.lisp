(in-package :cl-user)

(defpackage :clones.instructions
  (:use :cl :clones.addressing :clones.cpu)
  (:import-from :clones.memory
                :fetch
                :store
                :fetch-word)
  (:import-from :clones.util
                :*standard-optimize-settings*
                :wrap-byte
                :wrap-word
                :flip-bit)
  (:import-from :clones.instruction-data
                :%build-op-name
                :get-instruction-meta
                :jump-table)
  (:export #:single-step))

(in-package :clones.instructions)

(defmacro define-instruction (name () &body body)
  (let ((metadata (get-instruction-meta name)))
    (destructuring-bind (name opcodes docs &key access-pattern skip-pc) metadata
      (declare (ignore docs))
      `(progn
         ,@(loop for (opcode bytes cycles mode) in opcodes
                 ;; KLUDGE: Find the symbol since it doesn't exist at instruction-data load-time.
                 for addr-mode = (find-symbol (symbol-name mode))
                 collect `(%define-opcode (,name ,opcode ,addr-mode
                                           :bytes ,bytes
                                           :cycles ,cycles
                                           :access-pattern ,access-pattern
                                           :skip-pc ,skip-pc)
                            ,@body))
         ,@(loop for (opcode) in opcodes
                 collect `(export ',(%build-op-name name opcode) 'clones.instructions))))))

(defmacro %define-opcode ((name opcode address-mode &key bytes cycles access-pattern skip-pc)
                         &body body)
  `(defun ,(%build-op-name name opcode) (cpu)
     (declare (type cpu cpu))
     (declare #.*standard-optimize-settings*)
     (incf (cpu-pc cpu))
     ,(ecase access-pattern
        (:update (if (eql address-mode 'accumulator)
                     `(flet ((update (address value)
                               (setf (cpu-accum cpu) value)))
                        (let ((argument (accumulator cpu))
                              (address nil))
                          ,@body))
                     `(flet ((update (address value)
                               (store (cpu-memory cpu) address value)))
                        (let* ((address (,address-mode cpu))
                               (argument (fetch (cpu-memory cpu) address)))
                          ,@body))))
        (:pointer `(let ((address (,address-mode cpu)))
                     ,@body))
        (:dereference `(let ((argument (fetch (cpu-memory cpu) (,address-mode cpu))))
                         ,@body))
        ((nil) `(progn ,@body)))
     (incf (cpu-cycles cpu) ,cycles)
     ,@(unless (or skip-pc (= 1 bytes))
         `((incf (cpu-pc cpu) ,(1- bytes))))))

(defmacro branch-if (test)
  `(if ,test
       (progn
         (incf (cpu-cycles cpu))
         (setf (cpu-pc cpu) address))
       (incf (cpu-pc cpu))))

(define-instruction adc ()
  (let ((result (+ (cpu-accum cpu) argument (status-bit cpu :carry))))
    (set-flag-if cpu :overflow (overflow-p result (cpu-accum cpu) argument))
    (set-flag-if cpu :carry (> result #xff))
    (let ((wrapped (wrap-byte result)))
      (set-flags-zn cpu wrapped)
      (setf (cpu-accum cpu) wrapped))))

(define-instruction and ()
  (let ((result (setf (cpu-accum cpu) (logand (cpu-accum cpu) argument))))
    (set-flags-zn cpu result)))

(define-instruction asl ()
  (let ((result (wrap-byte (ash argument 1))))
    (set-flag-if cpu :carry (logbitp 7 argument))
    (set-flags-zn cpu result)
    (update address result)))

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

(define-instruction brk ()
  (let ((restore (wrap-word (1+ (cpu-pc cpu)))))
    (stack-push-word cpu restore)
    (set-flag cpu :break 1)
    (stack-push cpu (cpu-status cpu))
    (set-flag cpu :interrupt 1)
    (setf (cpu-pc cpu) (fetch-word (cpu-memory cpu) #xFFFE))))

(define-instruction bvc ()
  (branch-if (not (flag-set-p cpu :overflow))))

(define-instruction bvs ()
  (branch-if (flag-set-p cpu :overflow)))

(define-instruction clc ()
  (set-flag cpu :carry 0))

(define-instruction cld ()
  (set-flag cpu :decimal 0))

(define-instruction cli ()
  (set-flag cpu :interrupt 0))

(define-instruction clv ()
  (set-flag cpu :overflow 0))

(define-instruction cmp ()
  (compare cpu (cpu-accum cpu) argument))

(define-instruction cpx ()
  (compare cpu (cpu-x-reg cpu) argument))

(define-instruction cpy ()
  (compare cpu (cpu-y-reg cpu) argument))

(define-instruction dec ()
  (let ((result (wrap-byte (1- (fetch (cpu-memory cpu) argument)))))
    (set-flags-zn cpu result)
    (update address result)))

(define-instruction dex ()
  (let ((result (setf (cpu-x-reg cpu) (wrap-byte (1- (cpu-x-reg cpu))))))
    (set-flags-zn cpu result)))

(define-instruction dey ()
  (let ((result (setf (cpu-y-reg cpu) (wrap-byte (1- (cpu-y-reg cpu))))))
    (set-flags-zn cpu result)))

(define-instruction eor ()
  (let ((result (setf (cpu-accum cpu) (logxor (cpu-accum cpu) argument))))
    (set-flags-zn cpu result)))

(define-instruction inc ()
  (let ((result (wrap-byte (1+ (fetch (cpu-memory cpu) argument)))))
    (set-flags-zn cpu result)
    (update address result)))

(define-instruction inx ()
  (let ((result (wrap-byte (1+ (cpu-x-reg cpu)))))
    (setf (cpu-x-reg cpu) result)
    (set-flags-zn cpu result)))

(define-instruction iny ()
  (let ((result (wrap-byte (1+ (cpu-y-reg cpu)))))
    (setf (cpu-y-reg cpu) result)
    (set-flags-zn cpu result)))

(define-instruction jmp ()
  (setf (cpu-pc cpu) address))

(define-instruction jsr ()
  (stack-push-word cpu (1+ (cpu-pc cpu)))
  (setf (cpu-pc cpu) address))

(define-instruction lda ()
  (let ((result (setf (cpu-accum cpu) argument)))
    (set-flags-zn cpu result)))

(define-instruction ldx ()
  (let ((result (setf (cpu-x-reg cpu) argument)))
    (set-flags-zn cpu result)))

(define-instruction ldy ()
  (let ((result (setf (cpu-y-reg cpu) argument)))
    (set-flags-zn cpu result)))

(define-instruction lsr ()
  (declare (type fixnum argument))
  (let ((result (ash argument -1)))
    (set-flag-if cpu :carry (logbitp 0 argument))
    (set-flags-zn cpu result)
    (update address result)))

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

(define-instruction rol ()
  (declare (type fixnum argument))
  (let ((result (wrap-byte (ash argument 1))))
    (when (flag-set-p cpu :carry)
      (setf result (logior result #x01)))
    (set-flag-if cpu :carry (logbitp 7 argument))
    (set-flags-zn cpu result)
    (update address result)))

(define-instruction ror ()
  (declare (type fixnum argument))
  (let ((result (wrap-byte (ash argument -1))))
    (when (flag-set-p cpu :carry)
      (setf result (logior result #x80)))
    (set-flag-if cpu :carry (logbitp 0 argument))
    (set-flags-zn cpu result)
    (update address result)))

(define-instruction rti ()
  (setf (cpu-status cpu) (logior (stack-pop cpu) #x20))
  (setf (cpu-pc cpu) (stack-pop-word cpu)))

(define-instruction rts ()
  (setf (cpu-pc cpu) (1+ (stack-pop-word cpu))))

(define-instruction sbc ()
  (let* ((borrow-bit (flip-bit 0 (status-bit cpu :carry)))
         (result (- (cpu-accum cpu) argument borrow-bit)))
    (set-flag-if cpu :overflow (overflow-p result (cpu-accum cpu) (flip-bit 7 argument)))
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
  (update address (cpu-accum cpu)))

(define-instruction stx ()
  (update address (cpu-x-reg cpu)))

(define-instruction sty ()
  (update address (cpu-y-reg cpu)))

(define-instruction tax ()
  (let ((result (setf (cpu-x-reg cpu) (cpu-accum cpu))))
    (set-flags-zn cpu result)))

(define-instruction tay ()
  (let ((result (setf (cpu-y-reg cpu) (cpu-accum cpu))))
    (set-flags-zn cpu result)))

(define-instruction tsx ()
  (let ((result (setf (cpu-x-reg cpu) (cpu-stack cpu))))
    (set-flags-zn cpu result)))

(define-instruction txa ()
  (let ((result (setf (cpu-accum cpu) (cpu-x-reg cpu))))
    (set-flags-zn cpu result)))

(define-instruction txs ()
  (setf (cpu-stack cpu) (cpu-x-reg cpu)))

(define-instruction tya ()
  (let ((result (setf (cpu-accum cpu) (cpu-y-reg cpu))))
    (set-flags-zn cpu result)))

(defun single-step (cpu)
  "Execute a single instruction and return the CPU."
  (declare (type cpu cpu))
  (jump-table (fetch (cpu-memory cpu) (cpu-pc cpu))))
