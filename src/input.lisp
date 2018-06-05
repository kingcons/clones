(in-package :cl-user)

(defpackage :clones.input
  (:use :cl)
  (:export #:*keymap*
           #:gamepad
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

;; TODO: Would a better representation be a buttons bit-vector and strobe of indexes?

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

(defun update-pad (pad keycode state)
  (let ((key (sdl2:scancode-value keycode)))
    (cond ((sdl2:scancode= key (getf *keymap* :up))
           (setf (gamepad-up pad) state))
          ((sdl2:scancode= key (getf *keymap* :left))
           (setf (gamepad-left pad) state))
          ((sdl2:scancode= key (getf *keymap :down))
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

;; TODO: Will this work or do I need the :background option or plain WITH-SDL-EVENT?
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

(defun fetch-strobe (pad)
  (with-slots (strobe) pad
    (prog1 (slot-value pad strobe)
      (pop strobe))))

(defun reset-strobe (pad)
  (with-slots (strobe) pad
    (loop for button = (pop strobe)
          until (eq button 'right))))
