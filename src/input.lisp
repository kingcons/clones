(mgl-pax:define-package :clones.input
  (:use :cl :alexandria :mgl-pax))

(in-package :clones.input)

(defsection @input (:title "Input Handling")
  (controller class)
  (make-controller function)
  (read-controller function)
  (reset-controller function)
  (update-button function))

(defclass controller ()
  ((a       :type bit :initform 0 :accessor controller-a)
   (b       :type bit :initform 0 :accessor controller-b)
   (select  :type bit :initform 0 :accessor controller-select)
   (start   :type bit :initform 0 :accessor controller-start)
   (up      :type bit :initform 0 :accessor controller-up)
   (down    :type bit :initform 0 :accessor controller-down)
   (left    :type bit :initform 0 :accessor controller-left)
   (right   :type bit :initform 0 :accessor controller-right)
   (strobe  :type cons :accessor controller-strobe
            :initform '#0=(a b select start up down left right . #0#))))

(defun make-controller ()
  (make-instance 'controller))

(defun next-button (controller)
  (pop (controller-strobe controller)))

(defun read-controller (controller)
  (slot-value controller (next-button controller)))

(defun reset-controller (controller)
  (loop for button = (next-button controller)
        until (eq button 'right)))

(defun update-button (controller button value)
  (let ((slot-name (find-symbol (string button) :clones.input)))
    (setf (slot-value controller slot-name) value)))
