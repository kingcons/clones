(in-package :cl-user)

(defpackage :clones.graphics
  (:use :cl :clones.ppu))

(in-package :clones.graphics)

(defvar *screen-width* 256)
(defvar *screen-height* 240)

(defun randomize-buffer (buffer)
  (loop for i from 0 upto (1- (length buffer))
        do (setf (aref buffer i) (random 255))))

(defun new-frame (renderer framebuffer texture)
  (let ((start-of-frame (get-internal-real-time)))
    (randomize-buffer framebuffer)
    ;; see: https://wiki.libsdl.org/SDL_UpdateTexture
    (sdl2:update-texture texture (static-vectors:static-vector-pointer framebuffer)
                         :rect (cffi:null-pointer) :width (* 3 *screen-width*))
    (sdl2:render-copy renderer texture)
    (sdl2:render-present renderer)
    (format t "Frame drawn in ~A milliseconds~%" (- (get-internal-real-time) start-of-frame))))

(defun draw (&key (width *screen-width*) (height *screen-height*))
  (let ((size (* width height 3)))
    (static-vectors:with-static-vector (framebuffer size :element-type '(unsigned-byte 8))
      (sdl2:with-init (:video)
        (sdl2:with-window (window :title "Clones Demo" :w 256 :h 240)
          (sdl2:with-renderer (renderer window :flags '(:accelerated))
            (let ((texture (sdl2:create-texture renderer :bgr24 :streaming 256 240)))
              (sdl2:with-event-loop (:method :poll)
                (:keyup
                 (:keysym keysym)
                 (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                   (sdl2:push-event :quit))
                 (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
                   (new-frame renderer framebuffer texture)))
                (:idle
                 ()
                 )
                (:quit () t)))))))))

(defun test ()
  (sdl2:make-this-thread-main #'draw))
