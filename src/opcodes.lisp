(mgl-pax:define-package :clones.opcodes
  (:use :cl :alexandria :mgl-pax)
  (:import-from :serapeum
                #:octet))

(in-package :clones.opcodes)

(defsection @opcodes (:title "Opcode Data")
  (build-opcode-table function)
  (opcode-name structure-accessor)
  (opcode-size structure-accessor)
  (opcode-time structure-accessor)
  (opcode-addressing-mode structure-accessor)
  (opcode-access-pattern structure-accessor))

(defvar *opcodes*
  '((adc ((#x61 2 6 indirect-x)
          (#x65 2 3 zero-page)
          (#x69 2 2 immediate)
          (#x6d 3 4 absolute)
          (#x71 2 5 indirect-y)
          (#x75 2 4 zero-page-x)
          (#x79 3 4 absolute-y)
          (#x7d 3 4 absolute-x))
     :read)
    (and ((#x21 2 6 indirect-x)
          (#x25 2 3 zero-page)
          (#x29 2 2 immediate)
          (#x2d 3 4 absolute)
          (#x31 2 5 indirect-y)
          (#x35 2 4 zero-page-x)
          (#x39 3 4 absolute-y)
          (#x3d 3 4 absolute-x))
     :read)
    (asl ((#x06 2 5 zero-page)
          (#x0a 1 2 accumulator)
          (#x0e 3 6 absolute)
          (#x16 2 6 zero-page-x)
          (#x1e 3 7 absolute-x))
     :read-modify-write)
    (bit ((#x24 2 3 zero-page)
          (#x2c 3 4 absolute))
     :read)
    (cmp ((#xc1 2 6 indirect-x)
          (#xc5 2 3 zero-page)
          (#xc9 2 2 immediate)
          (#xcd 3 4 absolute)
          (#xd1 2 5 indirect-y)
          (#xd5 2 4 zero-page-x)
          (#xd9 3 4 absolute-y)
          (#xdd 3 4 absolute-x))
     :read)
    (cpx ((#xe0 2 2 immediate)
          (#xe4 2 3 zero-page)
          (#xec 3 4 absolute))
     :read)
    (cpy ((#xc0 2 2 immediate)
          (#xc4 2 3 zero-page)
          (#xcc 3 4 absolute))
     :read)
    (dec ((#xc6 2 5 zero-page)
          (#xce 3 6 absolute)
          (#xd6 2 6 zero-page-x)
          (#xde 3 7 absolute-x))
     :read-modify-write)
    (eor ((#x41 2 6 indirect-x)
          (#x45 2 3 zero-page)
          (#x49 2 2 immediate)
          (#x4d 3 4 absolute)
          (#x51 2 5 indirect-y)
          (#x55 2 4 zero-page-x)
          (#x59 3 4 absolute-y)
          (#x5d 3 4 absolute-x))
     :read)
    (inc ((#xe6 2 5 zero-page)
          (#xee 3 6 absolute)
          (#xf6 2 6 zero-page-x)
          (#xfe 3 7 absolute-x))
     :read-modify-write)
    (lda ((#xa1 2 6 indirect-x)
          (#xa5 2 3 zero-page)
          (#xa9 2 2 immediate)
          (#xad 3 4 absolute)
          (#xb1 2 5 indirect-y)
          (#xb5 2 4 zero-page-x)
          (#xb9 3 4 absolute-y)
          (#xbd 3 4 absolute-x))
     :read)
    (ldx ((#xa2 2 2 immediate)
          (#xa6 2 3 zero-page)
          (#xae 3 4 absolute)
          (#xb6 2 4 zero-page-y)
          (#xbe 3 4 absolute-y))
     :read)
    (ldy ((#xa0 2 2 immediate)
          (#xa4 2 3 zero-page)
          (#xac 3 4 absolute)
          (#xbc 3 4 absolute-x)
          (#xb4 2 4 zero-page-x))
     :read)
    (lsr ((#x46 2 5 zero-page)
          (#x4a 1 2 accumulator)
          (#x4e 3 6 absolute)
          (#x56 2 6 zero-page-x)
          (#x5e 3 7 absolute-x))
     :read-modify-write)
    (ora ((#x01 2 6 indirect-x)
          (#x05 2 3 zero-page)
          (#x09 2 2 immediate)
          (#x0d 3 4 absolute)
          (#x11 2 5 indirect-y)
          (#x15 2 4 zero-page-x)
          (#x19 3 4 absolute-y)
          (#x1d 3 4 absolute-x))
     :read)
    (rol ((#x2a 1 2 accumulator)
          (#x26 2 5 zero-page)
          (#x2e 3 6 absolute)
          (#x36 2 6 zero-page-x)
          (#x3e 3 7 absolute-x))
     :read-modify-write)
    (ror ((#x66 2 5 zero-page)
          (#x6a 1 2 accumulator)
          (#x6e 3 6 absolute)
          (#x76 2 6 zero-page-x)
          (#x7e 3 7 absolute-x))
     :read-modify-write)
    (sbc ((#xe1 2 6 indirect-x)
          (#xe5 2 3 zero-page)
          (#xe9 2 2 immediate)
          (#xed 3 4 absolute)
          (#xf1 2 5 indirect-y)
          (#xf5 2 4 zero-page-x)
          (#xf9 3 4 absolute-y)
          (#xfd 3 4 absolute-x))
     :read)
    (sta ((#x81 2 6 indirect-x)
          (#x85 2 3 zero-page)
          (#x8d 3 4 absolute)
          (#x91 2 6 indirect-y)
          (#x95 2 4 zero-page-x)
          (#x99 3 5 absolute-y)
          (#x9d 3 5 absolute-x))
     :write)
    (stx ((#x86 2 3 zero-page)
          (#x8e 3 4 absolute)
          (#x96 2 4 zero-page-y))
     :write)
    (sty ((#x84 2 3 zero-page)
          (#x8c 3 4 absolute)
          (#x94 2 4 zero-page-x))
     :write)
    (bcc ((#x90 2 2 relative))
     :jump)
    (bcs ((#xb0 2 2 relative))
     :jump)
    (beq ((#xf0 2 2 relative))
     :jump)
    (bmi ((#x30 2 2 relative))
     :jump)
    (bne ((#xd0 2 2 relative))
     :jump)
    (bpl ((#x10 2 2 relative))
     :jump)
    (bvc ((#x50 2 2 relative))
     :jump)
    (bvs ((#x70 2 2 relative))
     :jump)
    (jmp ((#x4c 3 3 absolute)
          (#x6c 3 5 indirect))
     :jump)
    (jsr ((#x20 3 6 absolute))
     :jump)
    (clc ((#x18 1 2 implied)) :static)
    (cld ((#xd8 1 2 implied)) :static)
    (cli ((#x58 1 2 implied)) :static)
    (clv ((#xb8 1 2 implied)) :static)
    (sec ((#x38 1 2 implied)) :static)
    (sed ((#xf8 1 2 implied)) :static)
    (sei ((#x78 1 2 implied)) :static)
    (pha ((#x48 1 3 implied)) :static)
    (php ((#x08 1 3 implied)) :static)
    (pla ((#x68 1 4 implied)) :static)
    (plp ((#x28 1 4 implied)) :static)
    (tax ((#xaa 1 2 implied)) :static)
    (tay ((#xa8 1 2 implied)) :static)
    (tsx ((#xba 1 2 implied)) :static)
    (txa ((#x8a 1 2 implied)) :static)
    (txs ((#x9a 1 2 implied)) :static)
    (tya ((#x98 1 2 implied)) :static)
    (brk ((#x00 1 7 implied)) :static)
    (dex ((#xca 1 2 implied)) :static)
    (dey ((#x88 1 2 implied)) :static)
    (inx ((#xe8 1 2 implied)) :static)
    (iny ((#xc8 1 2 implied)) :static)
    (nop ((#xea 1 2 implied)) :static)
    (rti ((#x40 1 6 implied)) :static)
    (rts ((#x60 1 6 implied)) :static))
  "An association list of instruction data of the form:
  (instruction-label ((code bytes cycles addressing-mode)*) access-pattern)")

(deftype addressing-mode ()
  "A list of keywords naming 6502 addressing modes."
  '(member :immediate :zero-page :zero-page-x :zero-page-y
    :accumulator :absolute :absolute-x :absolute-y
    :implied :indirect :indirect-x :indirect-y :relative))

(deftype access-pattern ()
  "A list of keywords naming access patterns for 6502 instructions."
  '(member :static :read :write :read-modify-write :jump))

(defstruct opcode
  (name 'fake :type symbol)
  (code 22 :type octet)
  (size 0 :type octet)
  (time 0 :type octet)
  (addressing-mode :implied :type addressing-mode)
  (access-pattern :static :type access-pattern))

(defun build-opcode-table ()
  (declare (optimize debug))
  (let ((table (make-array 256 :element-type 'opcode :initial-element (make-opcode))))
    (loop for (instruction opcodes access-pattern) in *opcodes*
          do (dolist (opcode opcodes)
               (destructuring-bind (code size time addressing-mode) opcode
                 (let ((opcode (make-opcode :name instruction
                                            :code code
                                            :size size
                                            :time time
                                            :addressing-mode (make-keyword addressing-mode)
                                            :access-pattern access-pattern)))
                   (setf (aref table code) opcode)))))
    table))