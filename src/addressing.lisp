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
                :cpu-cycles
                :page-crossed-p)
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
  (let* ((start (fetch-word memory program-counter))
         (final (wrap-word (+ start x-register))))
    (values final start)))

(defaddress absolute-y
  (let* ((start (fetch-word memory program-counter))
         (final (wrap-word (+ start y-register))))
    (values final start)))

(defaddress indirect
  (let ((start (fetch-word memory program-counter)))
    (fetch-indirect memory start)))

(defaddress indirect-x
  (let ((start (wrap-byte (+ (fetch memory program-counter) x-register))))
    (fetch-indirect memory start)))

(defaddress indirect-y
  (let* ((start (fetch-indirect memory (fetch memory program-counter)))
         (final (wrap-word (+ start y-register))))
    (values final start)))

;; Relative mode:
;; Offset is a signed byte in two's complement form.
;; If negative, xor the offset with 255 to determine the unsigned value and jump.
;; If positive, add one to offset to step over the offset and jump.
(defaddress relative
  (let ((offset (fetch memory program-counter)))
    (if (logbitp 7 offset)
        (wrap-word (- program-counter (logxor #xff offset)))
        (wrap-word (+ program-counter (1+ offset))))))
