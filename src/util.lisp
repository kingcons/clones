(mgl-pax:define-package :clones.util
  (:use :cl :mgl-pax))

(in-package :clones.util)

(defsection @util (:title "Assorted Utilities")
  (define-printer macro)
  (clear-buffer function)
  (wrap-byte function)
  (wrap-word function)
  (scale-2x function))

(defmacro define-printer (type (&rest vars) format-string &rest args)
  "Define printer is a helper macro for generating a PRINT-OBJECT
method. DEFINE-PRINTER provides a shorthand for the common case where
slot values need to be safely displayed but not read back in."
  `(defmethod print-object ((,type ,type) stream)
     (let ,(loop for v in vars
                 collect `(,v (handler-case (slot-value ,type ',v)
                                (unbound-slot () :unbound))))
       (print-unreadable-object (,type stream :type t)
         (let ((*print-pretty* nil))
           (format stream ,format-string ,@args))))))

(defun clear-buffer (buffer)
  (loop for i below (length buffer)
        do (setf (aref buffer i) 0)))

(defun wrap-byte (value)
  (declare (fixnum value))
  (ldb (byte 8 0) value))

(defun wrap-word (value)
  (declare (fixnum value))
  (ldb (byte 16 0) value))

(defun scale-2x (width height image-data)
  (let ((output (make-array (* width height 3) :element-type 'serapeum:octet)))
    (dotimes (h height)
      (dotimes (w width)
        (let ((offset (+ (* (floor width 2)
                            (floor h 2))
                         (floor w 2)))
              (scaled (+ (* width h) w)))
          (dotimes (pixel 3)
            (setf (aref output (+ (* scaled 3) pixel))
                  (aref image-data (+ (* offset 3) pixel)))))))
    output))
