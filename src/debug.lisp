(mgl-pax:define-package :clones.debug
  (:use :cl :alexandria :mgl-pax)
  (:import-from :serapeum #:~>>))

(in-package :clones.debug)

(defsection @debug (:title "Debugging Utilities")
  (dump-graphics function))

(defun for-sprites (ppu callback)
  (let ((oam (clones.ppu::ppu-oam ppu)))
    (dotimes (i 64)
      (let ((sprite (clones.ppu:make-sprite ppu (subseq oam i (+ i 4)))))
        (funcall callback sprite i)))))

(defun for-tile-scanlines (bytes callback)
  (loop for index below (/ (length bytes) 2)
        do (let ((low-byte (nth index bytes))
                 (high-byte (nth (+ index 8) bytes)))
             (funcall callback low-byte high-byte))))

(defun draw-sprite-to (ppu framebuffer coordinates sprite)
  (destructuring-bind (x-index y-index margin) coordinates
    (let ((tile-bytes (clones.ppu:fetch-tile-bytes ppu sprite))
          (high-bits (clones.ppu:palette-high-bits ppu sprite)))
      (flet ((draw-tile-line (low-byte high-byte)
               (dotimes (tile-index 8)
                 (let* ((low-bits (clones.ppu:palette-low-bits low-byte high-byte tile-index))
                        (palette-index (dpb high-bits (byte 2 2) low-bits))
                        (x-index (+ x-index tile-index margin)))
                   (clones.renderer::render-pixel framebuffer ppu y-index x-index palette-index)))
               (incf y-index)))
        (for-tile-scanlines tile-bytes #'draw-tile-line)))))

(defun coordinates-for (index &key (margin 4) (tile-width 8) (screen-width 256))
  (flet ((layout-fn (index)
           (floor (* index (+ tile-width (* margin 2)))
                  screen-width)))
    (multiple-value-bind (y-pos x-pos) (layout-fn index)
      (list x-pos (* y-pos (+ tile-width margin)) margin))))

(defun dump-graphics (ppu)
  (let ((framebuffer (clones.renderer::make-framebuffer)))
    (for-sprites ppu (lambda (sprite index)
                       (let ((coordinates (coordinates-for index)))
                         (draw-sprite-to ppu framebuffer coordinates sprite))))
    framebuffer))
