(mgl-pax:define-package :clones
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.renderer)
  (:import-from :static-vectors
                #:static-vector-pointer)
  (:import-from :clones.renderer
                #:*framebuffer*)
  (:import-from :clones.ppu
                #:ppu
                #:make-ppu))

(in-package :clones)

(defclass app ()
  ((cpu :initarg :cpu :type cpu :reader app-cpu)
   (paused :initform nil :type boolean :accessor app-paused)
   (renderer :initform nil :type (or null renderer) :accessor app-renderer))
  (:default-initargs
   :cpu (make-cpu)))

(defmethod initialize-instance :after ((app app) &key on-frame)
  (unless (app-renderer app)
    (with-slots (cpu) app
      (setf (app-renderer app)
            (make-renderer :ppu (cpu-ppu cpu)
                           :on-nmi (lambda () (nmi cpu))
                           :on-frame (or on-frame (constantly nil)))))))

(defparameter *debug* nil)

(defun make-on-frame (sdl-renderer texture last-frame-at)
  (lambda (ppu-renderer)
    (let* ((now (get-internal-real-time))
           (frame-time (- now last-frame-at)))
      (when *debug*
        (format t "Frame time: ~A~%" (floor frame-time 1000)))
      (setf last-frame-at now))
    (sdl2:render-clear sdl-renderer)
    (sdl2:update-texture texture (cffi:null-pointer) (static-vector-pointer *framebuffer*) (* 256 3))
    (sdl2:render-copy sdl-renderer texture)
    (sdl2:render-present sdl-renderer)))

(defgeneric run (app)
  (:documentation "Run the supplied APP.")
  (:method ((app app))
    (reset (app-cpu app))
    (sdl2:with-init (:everything)
      (sdl2:with-window (window :flags '(:shown :opengl))
        (sdl2:with-renderer (sdl-renderer window)
          (let ((texture (sdl2:create-texture sdl-renderer :rgb24 :streaming 256 240))
                (last-frame-at (get-internal-real-time)))
            (setf (clones.renderer::renderer-on-frame (app-renderer app))
                  (make-on-frame sdl-renderer texture last-frame-at))
            (sdl2:with-event-loop (:method :poll)
              (:keydown (:keysym keysym)
                (handle-input app keysym))
              (:idle ()
                (handle-idle app))
              (:quit () t))))))))

(defgeneric handle-input (app keysym)
  (:documentation "Take the appropriate action for KEYSYM in APP.")
  (:method ((app app) keysym)
    (let ((scancode (sdl2:scancode-value keysym)))
      (case (sdl2:scancode-symbol scancode)
        (:scancode-b (open-debugger app))
        (:scancode-h (print-help app))
        (:scancode-n (print-now app))
        (:scancode-p (toggle-pause app))
        (:scancode-escape (sdl2:push-event :quit))))))

(defun open-debugger (app)
  ;; TODO: Still can't access the app locals in this stack frame.
  (with-slots (cpu renderer) app
    (break)))

(defun print-help (app)
  (format t "
===========
CLONES HELP
===========
b: Trigger a break, opening the debugger.
h: Print this help message.
n: Print disassembly of the current instruction.
p: Pause or unpause the emulation.
ESC: Quit the app.
~%~%~%"))

(defun print-now (app)
  (with-slots (cpu) app
    (now cpu)))

(defun toggle-pause (app)
  (with-slots (cpu paused) app
    (format t "~%~:[Pausing ~;Unpausing ~] ~A~%~%" paused (cpu-cart cpu))
    (setf paused (not paused))))

(defgeneric handle-idle (app)
  (:documentation "Step the NES forward or perform other idle work.")
  (:method ((app app))
    (with-slots (cpu renderer paused) app
      (unless paused
        (single-step cpu)
        (sync renderer cpu)))))
