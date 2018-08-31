(in-package :cl-user)

(defpackage :clones.input
  (:use :cl)
  (:export #:gamepad
           #:make-gamepad
           #:handle-input
           #:fetch-strobe
           #:reset-strobe))

(in-package :clones.input)

(defvar *keymap*
  '(:up     :scancode-w
    :left   :scancode-a
    :down   :scancode-s
    :right  :scancode-d
    :a      :scancode-j
    :b      :scancode-k
    :select :scancode-space
    :start  :scancode-return))

(defstruct gamepad
  (up      0 :type bit)
  (left    0 :type bit)
  (down    0 :type bit)
  (right   0 :type bit)
  (a       0 :type bit)
  (b       0 :type bit)
  (select  0 :type bit)
  (start   0 :type bit)
  (strobe  '#0=(a b select start up down left right . #0#) :type cons))

(defmethod print-object ((gamepad gamepad) stream)
  (print-unreadable-object (gamepad stream :type t)
    (with-slots (up left down right a b select start) gamepad
      (format stream "↑:~d ↓:~d ←:~d →:~d A:~d B:~d Select:~d Start:~d"
              up down left right a b select start))))

(defun update-pad (pad keycode state)
  (let ((key (sdl2:scancode-value keycode)))
    (cond ((sdl2:scancode= key (getf *keymap* :up))
           (setf (gamepad-up pad) state))
          ((sdl2:scancode= key (getf *keymap* :left))
           (setf (gamepad-left pad) state))
          ((sdl2:scancode= key (getf *keymap* :down))
           (setf (gamepad-down pad) state))
          ((sdl2:scancode= key (getf *keymap* :right))
           (setf (gamepad-right pad) state))
          ((sdl2:scancode= key (getf *keymap* :a))
           (setf (gamepad-a pad) state))
          ((sdl2:scancode= key (getf *keymap* :b))
           (setf (gamepad-b pad) state))
          ((sdl2:scancode= key (getf *keymap* :select))
           (setf (gamepad-select pad) state))
          ((sdl2:scancode= key (getf *keymap* :start))
           (setf (gamepad-start pad) state)))))

(defun get-keysym (event)
  (plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym))

(defun get-key-name (keysym)
  (sdl2:scancode-name (sdl2:scancode-value keysym)))

(defun handle-input (gamepad)
  (sdl2:with-sdl-event (new-event)
    (loop for event = (sdl2:next-event new-event)
          until (zerop event)
          do (let ((event-type (sdl2:get-event-type new-event)))
               (case event-type
                 (:keydown
                  (let ((keysym (get-keysym new-event)))
                    ; (format t "Pressed key: ~S~%" (get-key-name keysym))
                    (update-pad gamepad keysym 1)))
                 (:keyup
                  (let ((keysym (get-keysym new-event)))
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (return :quit))
                    (update-pad gamepad keysym 0))))))))

(defun fetch-strobe (pad)
  (with-accessors ((strobe gamepad-strobe)) pad
    (slot-value pad (pop strobe))))

(defun reset-strobe (pad)
  (with-accessors ((strobe gamepad-strobe)) pad
    (loop for button = (pop strobe)
          until (eq button 'right))))
