(mgl-pax:define-package :clones
  (:use :cl :alexandria :mgl-pax)
  (:use :clones.cpu :clones.memory)
  (:import-from :clones.input
                #:update-button)
  (:import-from :clones.renderer
                #:renderer
                #:make-renderer
                #:sync)
  (:import-from :clones.util
                #:clear-buffer)
  (:import-from :static-vectors
                #:static-vector-pointer)
  (:import-from :serapeum
                #:~>>))

(in-package :clones)

(deftype framebuffer ()
  '(simple-array octet (184320)))

(defun make-framebuffer ()
  (let ((screen-width 256)
        (screen-height 240))
    (static-vectors:make-static-vector (* screen-width screen-height 3))))

(defclass app ()
  ((cpu :initarg :cpu :type cpu :reader app-cpu)
   (paused :initform nil :type boolean :accessor app-paused)
   (renderer :initform nil :type (or null renderer) :accessor app-renderer)
   (framebuffer :initform nil :type framebuffer :accessor app-framebuffer)
   (sdl-texture :initform nil :accessor app-sdl-texture)
   (sdl-renderer :initform nil :accessor app-sdl-renderer))
  (:default-initargs
   :cpu (make-cpu)))

(defmethod initialize-instance :after ((app app) &key on-frame)
  (unless (app-renderer app)
    (with-slots (cpu renderer framebuffer) app
      (setf renderer (make-renderer :ppu (~>> cpu cpu-memory memory-ppu)
                                    :on-nmi (lambda () (nmi cpu))
                                    :on-frame (or on-frame (constantly nil))))
      (setf framebuffer (make-framebuffer)))))

(defparameter *debug* nil)

(defun make-on-frame (app last-frame-at)
  (lambda (ppu-renderer)
    (declare (ignore ppu-renderer))
    (symbol-macrolet ((now (get-internal-real-time)))
      (let ((frame-time (floor (- now last-frame-at) 1000)))
        (when (< frame-time 16)
          (sdl2:delay (- 16 frame-time)))
        (when *debug*
          (format t "Frame time: ~A~%" (floor (- now last-frame-at) 1000)))
        (present-frame app)
        (setf last-frame-at now)))))

(defun present-frame (app)
  (with-accessors ((framebuffer app-framebuffer)
                   (sdl-texture app-sdl-texture)
                   (sdl-renderer app-sdl-renderer)) app
    (let ((buffer-pointer (static-vectors:static-vector-pointer framebuffer)))
      (sdl2:render-clear sdl-renderer)
      (sdl2:update-texture sdl-texture (cffi:null-pointer) buffer-pointer (* 256 3))
      (sdl2:render-copy sdl-renderer sdl-texture)
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
            (setf (app-sdl-renderer app) sdl-renderer
                  (app-sdl-texture app) texture)
            (setf (clones.renderer::renderer-on-frame (app-renderer app))
                  (make-on-frame app last-frame-at))
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
        (:scancode-h (print-help app))
        (:scancode-n (print-now app))
        (:scancode-i (step-instruction app))
        (:scancode-f (step-frame app))
        (:scancode-p (toggle-pause app))
        (:scancode-g (display-background app))
        (:scancode-r (display-sprites app))
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

(defun step-instruction (app)
  (with-slots (cpu) app
    (single-step cpu)
    (now cpu)))

(defun step-frame (app)
  (with-slots (cpu renderer framebuffer) app
    (loop with vblank-scanline = 241
          for cycles = (single-step cpu)
          for result = (sync renderer cpu framebuffer)
          until (eql result vblank-scanline))))

(defun print-help (app)
  (declare (ignore app))
  (format t "
===========
CLONES HELP
===========
# App controls
ESC: Quit the app.
h: Print this help message.
n: Print disassembly of the current instruction.
i: Step over the next CPU instruction.
f: Step forward one frame (until the next vblank).
p: Pause or unpause the emulation.
g: Display background. (use while paused)
r: Display sprites. (use while paused)
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

(defun display-background (app)
  (let ((ppu (~>> app app-cpu cpu-memory memory-ppu))
        (framebuffer (app-framebuffer app)))
    (clear-buffer framebuffer)
    (clones.debug:dump-graphics framebuffer ppu
                                :iterator #'clones.debug:for-background)
    (present-frame app)))

(defun display-sprites (app)
  (let* ((ppu (~>> app app-cpu cpu-memory memory-ppu))
         (framebuffer (app-framebuffer app)))
    (clear-buffer framebuffer)
    (clones.debug:dump-graphics framebuffer ppu
                                :iterator #'clones.debug:for-sprites
                                :margin 4)
    (present-frame app)))

(defun toggle-pause (app)
  (with-slots (cpu paused) app
    (let ((cartridge (~>> cpu cpu-memory memory-cart clones.mappers:mapper-pathname pathname-name)))
      (format t "~%~:[Pausing ~;Unpausing ~] ~A~%~%" paused cartridge)
      (setf paused (not paused)))))

(defgeneric handle-idle (app)
  (:documentation "Step the NES forward or perform other idle work.")
  (:method ((app app))
    (unless (app-paused app)
      (step-frame app))))
