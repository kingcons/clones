(in-package :cl-user)

(defpackage :clones.display
  (:use :cl)
  (:import-from :clones.ppu
                :*framebuffer*)
  (:import-from :static-vectors
                :static-vector-pointer)
  (:export #:init-display))

(in-package :clones.display)

(defvar *screen-width* 256)
(defvar *screen-height* 240)

(defvar *display* nil)
(defvar *renderer* nil)
(defvar *texture* nil)
(defvar *last-frame-at* nil)
(defvar *debug* nil)
(defvar *frame-count* 0)

(defun init-display ()
  (sdl2:in-main-thread ()
    (setf *display*  (sdl2:create-window :title "Clones"
                                         :w *screen-width*
                                         :h *screen-height*)
          *renderer* (sdl2:create-renderer *display* nil '(:accelerated))
          *texture*  (sdl2:create-texture *renderer* :rgb24 :streaming 256 240))))

(defun display-frame ()
  (sdl2:in-main-thread ()
    (sdl2:update-texture *texture* (static-vector-pointer *framebuffer*)
                         :rect (cffi:null-pointer)
                         :width (* 3 *screen-width*))
    (sdl2:render-clear *renderer*)
    (sdl2:render-copy *renderer* *texture*)
    (sdl2:render-present *renderer*)
    (incf *frame-count*)
    (when *debug*
      (format t "Frame: ~D~%" *frame-count*))
    (setf *last-frame-at* (get-internal-real-time))))

(defun test-frame ()
  (let* ((base (* 3 (random 64)))
         (red (aref clones.ppu::+color-palette+ (+ base 0)))
         (blue (aref clones.ppu::+color-palette+ (+ base 1)))
         (green (aref clones.ppu::+color-palette+ (+ base 2))))
    (loop for i from 0 upto (1- (* 256 240))
          do (setf (aref *framebuffer* (+ (* i 3) 0)) red
                   (aref *framebuffer* (+ (* i 3) 1)) blue
                   (aref *framebuffer* (+ (* i 3) 2)) green))
    (display-frame)))
