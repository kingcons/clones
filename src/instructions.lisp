(in-package :cl-user)

(defpackage :clones.instructions
  (:use :cl)
  (:export #:*instructions*
           #:*instructions-meta*
           #:initialize-metadata))

(in-package :cl-user)

(defvar *instructions-meta*
  (make-array #x100 :element-type list :initial-element '())
  "An array of opcodes -> (mnemonic bytes cycles address-mode docs).")

(defvar *instructions*
  '((adc ((#x61 2 6 indirect-x)
          (#x65 2 3 zero-page)
          (#x69 2 2 immediate)
          (#x6d 3 4 absolute)
          (#x71 2 5 indirect-y)
          (#x75 2 4 zero-page-x)
          (#x79 3 4 absolute-y)
          (#x7d 3 4 absolute-x))
     "Add with Carry")
    (and ((#x21 2 6 indirect-x)
          (#x25 2 3 zero-page)
          (#x29 2 2 immediate)
          (#x2d 3 4 absolute)
          (#x31 2 5 indirect-y)
          (#x35 2 4 zero-page-x)
          (#x39 3 4 absolute-y)
          (#x3d 3 4 absolute-x))
     "And with Accumulator")
    (asl ((#x06 2 5 zero-page)
          (#x0a 1 2 accumulator t)
          (#x0e 3 6 absolute)
          (#x16 2 6 zero-page-x)
          (#x1e 3 7 absolute-x))
     "Arithmetic Shift Left")
    (bit ((#x24 2 3 zero-page)
          (#x2c 3 4 absolute))
     "Test Bits with Accumulator")
    (cmp ((#xc1 2 6 indirect-x)
          (#xc5 2 3 zero-page)
          (#xc9 2 2 immediate)
          (#xcd 3 4 absolute)
          (#xd1 2 5 indirect-y)
          (#xd5 2 4 zero-page-x)
          (#xd9 3 4 absolute-y)
          (#xdd 3 4 absolute-x))
     "Compare with Accumulator")
    (cpx ((#xe0 2 2 immediate)
          (#xe4 2 3 zero-page)
          (#xec 3 4 absolute))
     "Compare with X Register")
    (cpy ((#xc0 2 2 immediate)
          (#xc4 2 3 zero-page)
          (#xcc 3 4 absolute))
     "Compare with Y Register")
    (dec ((#xc6 2 5 zero-page)
          (#xce 3 6 absolute)
          (#xd6 2 7 zero-page-x)
          (#xde 3 7 absolute-x))
     "Decrement Memory")
    (eor ((#x41 2 6 indirect-x)
          (#x45 2 3 zero-page)
          (#x49 2 2 immediate)
          (#x4d 3 4 absolute)
          (#x51 2 5 indirect-y)
          (#x55 2 4 zero-page-x)
          (#x59 3 4 absolute-y)
          (#x5d 3 4 absolute-x))
     "Exclusive OR with Accumulator")
    (inc ((#xe6 2 5 zero-page)
          (#xee 3 6 absolute)
          (#xf6 2 6 zero-page-x))
     "Increment Memory")
    (lda ((#xa1 2 6 indirect-x)
          (#xa5 2 3 zero-page)
          (#xa9 2 2 immediate)
          (#xad 3 4 absolute)
          (#xb1 2 5 indirect-y)
          (#xb5 2 4 zero-page-x)
          (#xb9 3 4 absolute-y)
          (#xbd 3 4 absolute-x))
     "Load into Accumulator")
    (ldx ((#xa2 2 2 immediate)
          (#xa6 2 3 zero-page)
          (#xae 3 4 absolute)
          (#xb6 2 4 zero-page-y)
          (#xbe 3 4 absolute-y))
     "Load into X Register")
    (ldy ((#xa0 2 2 immediate)
          (#xa4 2 3 zero-page)
          (#xac 3 4 absolute)
          (#xbc 3 4 absolute-x)
          (#xb4 2 4 zero-page-x))
     "Load into Y Register")
    (lsr ((#x46 2 5 zero-page)
          (#x4a 1 2 accumulator t)
          (#x4e 3 6 absolute)
          (#x56 2 6 zero-page-x)
          (#x5e 3 7 absolute-x))
     "Logical Shift Right")
    (ora ((#x01 2 6 indirect-x)
          (#x05 2 3 zero-page)
          (#x09 2 2 immediate)
          (#x0d 3 4 absolute)
          (#x11 2 5 indirect-y)
          (#x15 2 4 zero-page-x)
          (#x19 3 4 absolute-y)
          (#x1d 3 4 absolute-x))
     "Inclusive OR with Accumulator")
    (rol ((#x2a 1 2 accumulator t)
          (#x26 2 5 zero-page)
          (#x2e 3 6 absolute)
          (#x36 2 6 zero-page-x)
          (#x3e 3 7 absolute-x))
     "Rotate Left")
    (ror ((#x66 2 5 zero-page)
          (#x6a 1 2 accumulator t)
          (#x6e 3 6 absolute)
          (#x76 2 6 zero-page-x)
          (#x7e 3 7 absolute-x))
     "Rotate Right")
    (sbc ((#xe1 2 6 indirect-x)
          (#xe5 2 3 zero-page)
          (#xe9 2 2 immediate)
          (#xed 3 4 absolute)
          (#xf1 2 5 indirect-y)
          (#xf5 2 4 zero-page-x)
          (#xf9 3 4 absolute-y)
          (#xfd 3 4 absolute-x))
     "Subtract with Carry")
    (sta ((#x81 2 6 indirect-x)
          (#x85 2 3 zero-page)
          (#x8d 3 4 absolute)
          (#x91 2 6 indirect-y)
          (#x95 2 4 zero-page-x)
          (#x99 3 5 absolute-y)
          (#x9d 3 5 absolute-x))
     "Store Accumulator")
    (stx ((#x86 2 3 zero-page-x)
          (#x8e 3 4 absolute)
          (#x96 2 4 zero-page-y))
     "Store X Register")
    (sty ((#x84 2 3 zero-page)
          (#x8c 3 4 absolute)
          (#x96 2 4 zero-page-x))
     "Store Y Register")
    (bcc ((#x90 2 2 relative))
     "Branch on Carry Clear" t)
    (bcs ((#xb0 2 2 relative))
     "Branch on Carry Set" t)
    (beq ((#xf0 2 2 relative))
     "Branch on Equal" t)
    (bmi ((#x30 2 2 relative))
     "Branch on Minus (negative)" t)
    (bne ((#xd0 2 2 relative))
     "Branch on Not Equal" t)
    (bpl ((#x10 2 2 relative))
     "Branch on Plus (positive)" t)
    (bvc ((#x50 2 2 relative))
     "Branch on Overflow Clear" t)
    (bvs ((#x70 2 2 relative))
     "Branch on Overflow Set" t)
    (jmp ((#x4c 3 3 absolute t)
          (#x6c 3 5 indirect t))
     "Jump Unconditional" t)
    (jsr ((#x20 3 6 absolute t))
     "Jump Subroutine" t)
    (clc ((#x18 1 2 nil)) "Clear Carry Flag")
    (cld ((#xd8 1 2 nil)) "Clear Decimal Flag")
    (cli ((#x58 1 2 nil)) "Clear Interrupt Flag")
    (clv ((#xb8 1 2 nil)) "Clear Overflow Flag")
    (sec ((#x38 1 2 nil)) "Set Carry Flag")
    (sed ((#xf8 1 2 nil)) "Set Decimal Flag")
    (sei ((#x78 1 2 nil)) "Set Interrupt Flag")
    (pha ((#x48 1 3 nil)) "Push Accumulator")
    (php ((#x08 1 3 nil)) "Push Status")
    (pla ((#x68 1 4 nil)) "Pull Accumulator")
    (plp ((#x28 1 4 nil)) "Pull Status")
    (tax ((#xaa 1 2 nil)) "Transfer Accumulator -> X")
    (tay ((#xa8 1 2 nil)) "Transfer Accumulator -> Y")
    (tsx ((#xba 1 2 nil)) "Transfer Stack Pointer -> X")
    (txa ((#x8a 1 2 nil)) "Transfer X -> Accumulator")
    (txs ((#x9a 1 2 nil)) "Transfer X -> Stack Pointer")
    (tya ((#x98 1 2 nil)) "Transfer Y -> Accumulator")
    (brk ((#x00 1 7 nil)) "Break")
    (dex ((#xca 1 2 nil)) "Decrement X")
    (dey ((#x88 1 2 nil)) "Decrement Y")
    (inx ((#xe8 1 2 nil)) "Increment X")
    (iny ((#xc8 1 2 nil)) "Increment Y")
    (nop ((#xea 1 2 nil)) "No Operation")
    (rti ((#x40 1 6 nil)) "Return from Interrupt")
    (rts ((#x60 1 6 nil)) "Return from Subroutine"))
  "A list describing the 6502 Instruction Set of the form:
  (assembly-mnemonic ((opcode-1 bytes cycles addressing-mode &optional raw)
                      (opcode-n bytes cycles addressing-mode &optional raw))
                     description &optional skip-pc-bump)")

(defun initialize-metadata ()
  (loop for (name versions description skip-pc) in *instructions*
        do (loop for (opcode bytes cycles addr-mode) in versions
                 do (setf (aref *instructions-meta* opcode)
                          `(,name ,bytes ,cycles ,addr-mode ,description)))))
