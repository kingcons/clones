(mgl-pax:define-package :clones
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.memory)
  (:import-from :clones.input
                #:update-button)
  (:import-from :clones.renderer
                #:make-renderer
                #:sync)
  (:import-from :static-vectors
                #:static-vector-pointer)
  (:import-from :serapeum
                #:~>>))

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
            (make-renderer :ppu (~>> cpu cpu-memory memory-ppu)
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
    (let ((framebuffer (static-vector-pointer clones.renderer:*framebuffer*)))
      (sdl2:render-clear sdl-renderer)
      (sdl2:update-texture texture (cffi:null-pointer) framebuffer (* 256 3))
      (sdl2:render-copy sdl-renderer texture)
      (sdl2:render-present sdl-renderer))))

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
                (handle-keydown app keysym))
              (:keyup (:keysym keysym)
                (handle-keyup app keysym))
              (:idle ()
                (handle-idle app))
              (:quit () t))))))))

(defgeneric handle-keydown (app keysym)
  (:documentation "Take the appropriate action for KEYSYM in APP.")
  (:method ((app app) keysym)
    (let ((controller (~>> app app-cpu cpu-memory memory-controller)))
      (case (sdl2:scancode keysym)
        (:scancode-b (open-debugger app))
        (:scancode-h (print-help app))
        (:scancode-n (print-now app))
        (:scancode-i (step-instruction app))
        (:scancode-f (step-frame app))
        (:scancode-p (toggle-pause app))
        (:scancode-w (update-button controller 'up 1))
        (:scancode-s (update-button controller 'down 1))
        (:scancode-a (update-button controller 'left 1))
        (:scancode-d (update-button controller 'right 1))
        (:scancode-j (update-button controller 'a 1))
        (:scancode-k (update-button controller 'b 1))
        (:scancode-return (update-button controller 'start 1))
        (:scancode-space (update-button controller 'select 1))
        (:scancode-escape (sdl2:push-event :quit))))))

(defgeneric handle-keyup (app keysym)
  (:documentation "Perform any special handling for releasing KEYSYM in APP.")
  (:method ((app app) keysym)
    (let ((controller (~>> app app-cpu cpu-memory memory-controller)))
      (case (sdl2:scancode keysym)
        (:scancode-w (update-button controller 'up 0))
        (:scancode-s (update-button controller 'down 0))
        (:scancode-a (update-button controller 'left 0))
        (:scancode-d (update-button controller 'right 0))
        (:scancode-j (update-button controller 'a 0))
        (:scancode-k (update-button controller 'b 0))
        (:scancode-return (update-button controller 'start 0))
        (:scancode-space (update-button controller 'select 0))))))

(defun open-debugger (app)
  ;; TODO: Still can't access the app locals in this stack frame.
  (with-slots (cpu renderer) app
    (break)))

(defun step-instruction (app)
  (with-slots (cpu) app
    (single-step cpu)
    (now cpu)))

(defun step-frame (app)
  (with-slots (cpu renderer) app
    (loop with vblank-scanline = 241
          for cycles = (single-step cpu)
          for result = (sync renderer cpu)
          until (eql result vblank-scanline))))

(defun print-help (app)
  (format t "
===========
CLONES HELP
===========
# App controls
ESC: Quit the app.
b: Trigger a break, opening the debugger.
h: Print this help message.
n: Print disassembly of the current instruction.
i: Step over the next CPU instruction.
f: Step forward one frame (until the next vblank).
p: Pause or unpause the emulation.
# Game controls
w: Up
s: Down
a: Left
d: Right
j: A
k: B
Space: Select
Enter: Start
~%~%~%"))

(defun print-now (app)
  (with-slots (cpu) app
    (now cpu)))

(defun toggle-pause (app)
  (with-slots (cpu paused) app
    (let ((cartridge (~>> cpu cpu-memory memory-cart clones.mappers:mapper-pathname pathname-name)))
      (format t "~%~:[Pausing ~;Unpausing ~] ~A~%~%" paused cartridge)
      (setf paused (not paused)))))

(defgeneric handle-idle (app)
  (:documentation "Step the NES forward or perform other idle work.")
  (:method ((app app))
    (with-slots (cpu renderer paused) app
      (unless paused
        (single-step cpu)
        (sync renderer cpu)))))
