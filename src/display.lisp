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

(defun init-display ()
  (sdl2:in-main-thread ()
    (sdl2:init :everything)
    (setf *display*  (sdl2:create-window :title "Clones"
                                         :w *screen-width*
                                         :h *screen-height*)
          *renderer* (sdl2:create-renderer *display*
                                           :flags '(:accelerated))
          *texture*  (sdl2:create-texture renderer :rgb24 :streaming 256 240))))

(defun display-frame ()
  (sdl2:in-main-thread ()
    (sdl2:update-texture *texture* (static-vector-pointer *framebuffer*)
                         :rect (cffi:null-pointer)
                         :width (* 3 *screen-width*))
    (sdl2:render-copy *renderer* *texture*)
    (sdl2:render-present *renderer*)
    (when *last-frame-at*
      (format t "Frame drawn in ~A milliseconds~%" (- (get-internal-real-time) *last-frame-at*)))
    (setf *last-frame-at* (get-internal-real-time))))
