(in-package :cl-user)

(defpackage :clones.input
  (:use :cl)
  (:export #:gamepad
           #:make-gamepad
           #:handle-input))

(in-package :clones.input)

(defstruct gamepad
  (up      0 :type bit)
  (left    0 :type bit)
  (down    0 :type bit)
  (right   0 :type bit)
  (a       0 :type bit)
  (b       0 :type bit)
  (select  0 :type bit)
  (start   0 :type bit))

(defun update-pad (pad keycode state)
  (let ((key (sdl2:scancode-value keycode)))
    (cond ((sdl2:scancode= key :scancode-w)
           (setf (gamepad-up pad) state))
          ((sdl2:scancode= key :scancode-a)
           (setf (gamepad-left pad) state))
          ((sdl2:scancode= key :scancode-s)
           (setf (gamepad-down pad) state))
          ((sdl2:scancode= key :scancode-d)
           (setf (gamepad-right pad) state))
          ((sdl2:scancode= key :scancode-j)
           (setf (gamepad-a pad) state))
          ((sdl2:scancode= key :scancode-k)
           (setf (gamepad-b pad) state))
          ((sdl2:scancode= key :scancode-space)
           (setf (gamepad-select pad) state))
          ((sdl2:scancode= key :scancode-return)
           (setf (gamepad-start pad) state)))))

(defun handle-input (gamepad)
  (sdl2:with-event-loop (:method :poll)
    (:keydown
     (:keysym keysym)
     (update-pad gamepad keysym 1))
    (:keyup
     (:keysym keysym)
     (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
         (sdl2:push-event :quit)
         (update-pad gamepad keysym 0)))
    (:quit () t)))
