(in-package :cl-user)

(defpackage :clones.cpu
  (:use :cl :clones.memory)
  (:import-from :clones.util
                :ub8
                :ub16)
  (:export #:cpu
           #:make-cpu
           #:cpu-memory
           #:cpu-cycles
           #:cpu-accum
           #:cpu-x-reg
           #:cpu-y-reg
           #:cpu-stack
           #:cpu-status
           #:cpu-pc))

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
