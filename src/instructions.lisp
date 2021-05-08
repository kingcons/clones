(in-package :cl-user)

(defpackage :clones.instructions
  (:use :cl :clones.addressing :clones.cpu :clones.instruction-data)
  (:import-from :clones.memory
                :fetch
                :store
                :fetch-word)
  (:import-from :clones.util
                :*standard-optimize-settings*
                :wrap-byte
                :wrap-word
                :flip-bit)
  (:export #:single-step))

(in-package :clones.instructions)

(defmacro with-address (&body body)
  "Wraps BODY with a lexical definition of an ADDRESS variable.
Expects CPU and ADDR-MODE to be lexically present variables."
  `(let ((address (funcall addr-mode cpu)))
     ,@body))

(defmacro with-argument (&body body)
  `(multiple-value-bind (final start) (funcall addr-mode cpu)
     (let ((argument (fetch (cpu-memory cpu) final)))
       (when (member addr-mode '(absolute-x absolute-y indirect-y))
         (maybe-update-cycle-count cpu start final))
       ,@body)))

(defmacro with-update (&body body)
  `(flet ((update (address value)
            (if (eql addr-mode 'accumulator)
                (setf (cpu-accum cpu) value)
                (store (cpu-memory cpu) address value))))
     (let* ((address (funcall addr-mode cpu))
            (argument (if (eql addr-mode 'accumulator)
                          (funcall addr-mode cpu)
                          (fetch (cpu-memory cpu) address))))
       ,@body)))

(defmacro define-instruction (name () &body body)
  `(defun ,(alexandria:symbolicate 'op- name) (cpu addr-mode)
     (declare (type cpu cpu))
     (declare #.*standard-optimize-settings*)
     ,@body))

(defmacro branch-if (test)
  `(if ,test
       (progn
         (incf (cpu-cycles cpu))
         (setf (cpu-pc cpu) address))
       (incf (cpu-pc cpu))))

(define-instruction adc ()
  (with-argument
    (let ((result (+ (cpu-accum cpu) argument (status-bit cpu :carry))))
      (set-flag-if cpu :overflow (overflow-p result (cpu-accum cpu) argument))
      (set-flag-if cpu :carry (> result #xff))
      (let ((wrapped (wrap-byte result)))
        (set-flags-zn cpu wrapped)
        (setf (cpu-accum cpu) wrapped)))))

(define-instruction and ()
  (with-argument
    (let ((result (setf (cpu-accum cpu) (logand (cpu-accum cpu) argument))))
      (set-flags-zn cpu result))))

(define-instruction asl ()
  (with-update
    (let ((result (wrap-byte (ash argument 1))))
      (set-flag-if cpu :carry (logbitp 7 argument))
      (set-flags-zn cpu result)
      (update address result))))

(define-instruction bcc ()
  (with-address
    (branch-if (not (flag-set-p cpu :carry)))))

(define-instruction bcs ()
  (with-address
    (branch-if (flag-set-p cpu :carry))))

(define-instruction beq ()
  (with-address
    (branch-if (flag-set-p cpu :zero))))

(define-instruction bit ()
  (with-argument
    (set-flag-if cpu :zero (zerop (logand (cpu-accum cpu) argument)))
    (set-flag-if cpu :negative (logbitp 7 argument))
    (set-flag-if cpu :overflow (logbitp 6 argument))))

(define-instruction bmi ()
  (with-address
    (branch-if (flag-set-p cpu :negative))))

(define-instruction bne ()
  (with-address
    (branch-if (not (flag-set-p cpu :zero)))))

(define-instruction bpl ()
  (with-address
    (branch-if (not (flag-set-p cpu :negative)))))

(define-instruction brk ()
  (let ((restore (wrap-word (1+ (cpu-pc cpu)))))
    (stack-push-word cpu restore)
    (set-flag cpu :break 1)
    (stack-push cpu (cpu-status cpu))
    (set-flag cpu :interrupt 1)
    (setf (cpu-pc cpu) (fetch-word (cpu-memory cpu) #xFFFE))))

(define-instruction bvc ()
  (with-address
    (branch-if (not (flag-set-p cpu :overflow)))))

(define-instruction bvs ()
  (with-address
    (branch-if (flag-set-p cpu :overflow))))

(define-instruction clc ()
  (set-flag cpu :carry 0))

(define-instruction cld ()
  (set-flag cpu :decimal 0))

(define-instruction cli ()
  (set-flag cpu :interrupt 0))

(define-instruction clv ()
  (set-flag cpu :overflow 0))

(define-instruction cmp ()
  (with-argument
    (compare cpu (cpu-accum cpu) argument)))

(define-instruction cpx ()
  (with-argument
    (compare cpu (cpu-x-reg cpu) argument)))

(define-instruction cpy ()
  (with-argument
    (compare cpu (cpu-y-reg cpu) argument)))

(define-instruction dec ()
  (with-update
    (let ((result (wrap-byte (1- argument))))
      (set-flags-zn cpu result)
      (update address result))))

(define-instruction dex ()
  (let ((result (setf (cpu-x-reg cpu) (wrap-byte (1- (cpu-x-reg cpu))))))
    (set-flags-zn cpu result)))

(define-instruction dey ()
  (let ((result (setf (cpu-y-reg cpu) (wrap-byte (1- (cpu-y-reg cpu))))))
    (set-flags-zn cpu result)))

(define-instruction eor ()
  (with-argument
    (let ((result (setf (cpu-accum cpu) (logxor (cpu-accum cpu) argument))))
      (set-flags-zn cpu result))))

(define-instruction inc ()
  (with-update
    (let ((result (wrap-byte (1+ argument))))
      (set-flags-zn cpu result)
      (update address result))))

(define-instruction inx ()
  (let ((result (wrap-byte (1+ (cpu-x-reg cpu)))))
    (setf (cpu-x-reg cpu) result)
    (set-flags-zn cpu result)))

(define-instruction iny ()
  (let ((result (wrap-byte (1+ (cpu-y-reg cpu)))))
    (setf (cpu-y-reg cpu) result)
    (set-flags-zn cpu result)))

(define-instruction jmp ()
  (with-address
    (setf (cpu-pc cpu) address)))

(define-instruction jsr ()
  (with-address
    (stack-push-word cpu (1+ (cpu-pc cpu)))
    (setf (cpu-pc cpu) address)))

(define-instruction lda ()
  (with-argument
    (let ((result (setf (cpu-accum cpu) argument)))
      (set-flags-zn cpu result))))

(define-instruction ldx ()
  (with-argument
    (let ((result (setf (cpu-x-reg cpu) argument)))
      (set-flags-zn cpu result))))

(define-instruction ldy ()
  (with-argument
    (let ((result (setf (cpu-y-reg cpu) argument)))
      (set-flags-zn cpu result))))

(define-instruction lsr ()
  (with-update
    (declare (type fixnum argument))
    (let ((result (ash argument -1)))
      (set-flag-if cpu :carry (logbitp 0 argument))
      (set-flags-zn cpu result)
      (update address result))))

(define-instruction nop ()
  nil)

(define-instruction ora ()
  (with-argument
    (let ((result (setf (cpu-accum cpu) (logior (cpu-accum cpu) argument))))
      (set-flags-zn cpu result))))

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
  (with-update
    (declare (type fixnum argument))
    (let ((result (wrap-byte (ash argument 1))))
      (when (flag-set-p cpu :carry)
        (setf result (logior result #x01)))
      (set-flag-if cpu :carry (logbitp 7 argument))
      (set-flags-zn cpu result)
      (update address result))))

(define-instruction ror ()
  (with-update
    (declare (type fixnum argument))
    (let ((result (wrap-byte (ash argument -1))))
      (when (flag-set-p cpu :carry)
        (setf result (logior result #x80)))
      (set-flag-if cpu :carry (logbitp 0 argument))
      (set-flags-zn cpu result)
      (update address result))))

(define-instruction rti ()
  (setf (cpu-status cpu) (logior (stack-pop cpu) #x20))
  (setf (cpu-pc cpu) (stack-pop-word cpu)))

(define-instruction rts ()
  (setf (cpu-pc cpu) (1+ (stack-pop-word cpu))))

(define-instruction sbc ()
  (with-argument
    (let* ((borrow-bit (flip-bit 0 (status-bit cpu :carry)))
           (result (- (cpu-accum cpu) argument borrow-bit)))
      (set-flag-if cpu :overflow (overflow-p result (cpu-accum cpu) (flip-bit 7 argument)))
      (set-flag-if cpu :carry (>= result 0))
      (let ((wrapped (wrap-byte result)))
        (set-flags-zn cpu wrapped)
        (setf (cpu-accum cpu) wrapped)))))

(define-instruction sec ()
  (set-flag cpu :carry 1))

(define-instruction sed ()
  (set-flag cpu :decimal 1))

(define-instruction sei ()
  (set-flag cpu :interrupt 1))

(define-instruction sta ()
  (with-address
    (store (cpu-memory cpu) address (cpu-accum cpu))))

(define-instruction stx ()
  (with-address
    (store (cpu-memory cpu) address (cpu-x-reg cpu))))

(define-instruction sty ()
  (with-address
    (store (cpu-memory cpu) address (cpu-y-reg cpu))))

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
  (declare (type cpu cpu))
  (let ((opcode (fetch (cpu-memory cpu) (cpu-pc cpu))))
    (with-slots (addr-mode byte-count cycle-count handler pattern)
        (aref *opcodes* opcode)
      (incf (cpu-pc cpu))
      (funcall handler cpu (and addr-mode (fdefinition addr-mode)))
      (unless (eql pattern :jump)
        (incf (cpu-pc cpu) (1- byte-count)))
      (incf (cpu-cycles cpu) cycle-count))))
