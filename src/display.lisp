(in-package :cl-user)

(defpackage :clones.display
  (:use :cl)
  (:import-from :clones.ppu
                :*framebuffer*)
  (:import-from :static-vectors
                :static-vector-pointer)
  (:export #:init-display
           #:display-frame
           #:*frame-count*))

(in-package :clones.display)

(defvar *screen-width* 512)
(defvar *screen-height* 480)

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
    (sdl2:update-texture *texture* (static-vector-pointer *framebuffer*)
                         :rect (cffi:null-pointer)
                         :width (* 3 256))
    (sdl2:render-clear *renderer*)
    (sdl2:render-copy *renderer* *texture*)
    (incf *frame-count*)
    (let* ((new-frame-at (get-internal-real-time))
           (draw-duration (- new-frame-at *last-frame-at*)))
      (when *debug*
        (format t "Frame ready in ~A milliseconds~%" draw-duration))
      (when (< draw-duration 15)
        (sdl2:delay (- 15 draw-duration)))
      (sdl2:render-present *renderer*)
      (setf *last-frame-at* (get-internal-real-time)))))
