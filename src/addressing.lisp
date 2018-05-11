(in-package :cl-user)

(defpackage :clones.addressing
  (:use :cl)
  (:import-from :clones.cpu
                :cpu
                :cpu-pc
                :cpu-accum
                :cpu-x-reg
                :cpu-y-reg
                :cpu-memory
                :cpu-cycles)
  (:import-from :clones.memory
                :fetch
                :fetch-word
                :fetch-indirect)
  (:import-from :clones.util
                :wrap-byte
                :wrap-word
                :ub8
                :ub16)
  (:export #:immediate
           #:accumulator
           #:zero-page
           #:zero-page-x
           #:zero-page-y
           #:absolute
           #:absolute-x
           #:absolute-y
           #:indirect
           #:indirect-x
           #:indirect-y
           #:relative))

(in-package :clones.addressing)

(defmacro defaddress (name &body body)
  `(defun ,name (cpu)
     (declare (type cpu cpu))
     (symbol-macrolet ((memory          (cpu-memory cpu))
                       (program-counter (cpu-pc cpu))
                       (accumulator     (cpu-accum cpu))
                       (x-register      (cpu-x-reg cpu))
                       (y-register      (cpu-y-reg cpu)))
       ,@body)))

(defaddress immediate
  program-counter)

(defaddress accumulator
  accumulator)

(defaddress zero-page
  (fetch memory program-counter))

(defaddress zero-page-x
  (let ((start (fetch memory program-counter)))
    (wrap-byte (+ start x-register))))

(defaddress zero-page-y
  (let ((start (fetch memory program-counter)))
    (wrap-byte (+ start y-register))))

(defaddress absolute
  (fetch-word memory program-counter))

(defaddress absolute-x
  (wrap-word (+ x-register (fetch-word memory program-counter))))

(defaddress absolute-y
  (wrap-word (+ y-register (fetch-word memory program-counter))))

(defaddress indirect
  (let ((start (fetch-word memory program-counter)))
    (fetch-indirect memory start)))

;; TODO: Seems likely we need to wrap-word after additions here.
(defaddress indirect-x
  (let ((start (+ (fetch-word memory program-counter) x-register)))
    (fetch-indirect memory start)))

(defaddress indirect-y
  (let ((start (fetch-word memory program-counter)))
    (+ y-register (fetch-indirect memory start))))

;; Relative mode:
;; Offset is a signed byte in two's complement form.
;; If negative, xor the offset with 255 to determine the unsigned value and jump.
;; If positive, add one to offset to step over the offset and jump.
(defaddress relative
  (let* ((offset (fetch memory program-counter))
         (result (if (logbitp 7 offset)
                     (wrap-word (- program-counter (logxor #xff offset)))
                     (wrap-word (+ program-counter (1+ offset))))))
    (incf (cpu-cycles cpu))
    result))
