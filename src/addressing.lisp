(in-package :cl-user)

(defpackage :clones.addressing
  (:use :cl)
  (:import-from :clones.cpu
                :cpu-pc
                :cpu-accum
                :cpu-x-reg
                :cpu-y-reg
                :cpu-memory)
  (:import-from :clones.memory
                :fetch
                :fetch-word
                :fetch-indirect)
  (:import-from :clones.util
                :wrap-byte
                :wrap-word)
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
           #:relative
           #:get-format-string))

(in-package :clones.addressing)

(defmacro defaddress (name (&key writer) &body body)
  `(progn
     (defun ,name (cpu)
       (symbol-macrolet ((memory          (cpu-memory cpu))
                         (program-counter (cpu-pc cpu))
                         (accumulator     (cpu-accum cpu))
                         (x-register      (cpu-x-reg cpu))
                         (y-register      (cpu-y-reg cpu)))
         ,@body))
     (setf (get ',name 'writer) ,writer)))

(defun get-format-string (mode)
  (get (find-symbol (symbol-name mode) 'clones.addressing) 'writer))

(defaddress immediate (:writer "~{#$~2,'0x~}")
  program-counter)

(defaddress accumulator (:writer "A")
  accumulator)

(defaddress zero-page (:writer "~{$~2,'0x~}")
  (fetch memory program-counter))

(defaddress zero-page-x (:writer "$~{~2,'0x~}, X")
  (let ((start (fetch memory program-counter)))
    (wrap-byte (+ start x-register))))

(defaddress zero-page-y (:writer "$~{~2,'0x~}, Y")
  (let ((start (fetch memory program-counter)))
    (wrap-byte (+ start y-register))))

(defaddress absolute (:writer "$~{~2,'0x~}")
  (fetch-word memory program-counter))

(defaddress absolute-x (:writer "$~{~2,'0x~}, X")
  (let* ((start (fetch-word memory program-counter))
         (final (wrap-word (+ start x-register))))
    (values final start)))

(defaddress absolute-y (:writer "$~{~2,'0x~}, Y")
  (let* ((start (fetch-word memory program-counter))
         (final (wrap-word (+ start y-register))))
    (values final start)))

(defaddress indirect (:writer "($~{~2,'0x~})")
  (let ((start (fetch-word memory program-counter)))
    (fetch-indirect memory start)))

(defaddress indirect-x (:writer "($~{~2,'0x~}), X")
  (let ((start (wrap-byte (+ (fetch memory program-counter) x-register))))
    (fetch-indirect memory start)))

(defaddress indirect-y (:writer "($~{~2,'0x~}), Y")
  (let* ((start (fetch-indirect memory (fetch memory program-counter)))
         (final (wrap-word (+ start y-register))))
    (values final start)))

;; Relative mode:
;; Offset is a signed byte in two's complement form.
;; If negative, xor the offset with 255 to determine the unsigned value and jump.
;; If positive, add one to offset to step over the offset and jump.
(defaddress relative (:writer "&~{~2,'0x~}")
  (let ((offset (fetch memory program-counter)))
    (if (logbitp 7 offset)
        (wrap-word (- program-counter (logxor #xff offset)))
        (wrap-word (+ program-counter (1+ offset))))))
