(in-package :cl-user)

(defpackage :clones.display
  (:use :cl)
  (:import-from :clones.render
                :*framebuffer*
                :+width+
                :+height+)
  (:import-from :static-vectors
                :static-vector-pointer)
  (:export #:init-display
           #:display-frame))

(in-package :clones.display)

(defvar *screen-width* (* 2 +width+))
(defvar *screen-height* (* 2 +height+))

(defvar *display* nil)
(defvar *renderer* nil)
(defvar *texture* nil)
(defvar *last-frame-at* 0.0)
(defvar *debug* t)
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
    (sdl2:update-texture *texture*
                         (cffi:null-pointer)
                         (static-vector-pointer *framebuffer*)
                         (* 3 256))
    (sdl2:render-clear *renderer*)
    (sdl2:render-copy *renderer* *texture*)
    (incf *frame-count*)
    (let ((draw-duration (- (get-internal-real-time) *last-frame-at*)))
      (when *debug*
        (format t "Frame ready in ~A milliseconds~%" draw-duration))
      (when (< draw-duration 16)
        (sdl2:delay (- 16 draw-duration)))
      (setf *last-frame-at* (get-internal-real-time))
      (sdl2:render-present *renderer*))))
