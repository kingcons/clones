(mgl-pax:define-package :clones.debug
  (:use :cl :alexandria :mgl-pax)
  (:import-from :serapeum #:~>>))

(in-package :clones.debug)

(defsection @debug (:title "Debugging Utilities")
  (for-sprites function)
  (for-background function)
  (dump-graphics function))

(defun with-ppu-address (ppu new-address callback)
  (let ((origin (clones.ppu::ppu-address ppu)))
    ;; TODO: Do we need to also backup CTRL register here?
    (setf (clones.ppu::ppu-address ppu) new-address)
    (funcall callback ppu)
    (setf (clones.ppu::ppu-address ppu) origin)))

(defun for-sprites (ppu callback)
  (dotimes (i 64)
    (let ((sprite (clones.ppu:make-sprite ppu i)))
      (funcall callback sprite i))))

(defun for-background (ppu callback &key (name-table 0))
  ;; TODO: How can we pass keyword arguments to FOR-BACKGROUND without propagating
  ;; the args from all iterators to be keyword arguments to dump-graphics?
  ;; Can &allow-key help with this even though dump-graphics is not a method?
  (with-ppu-address ppu (* #x400 name-table)
    (lambda (ppu)
      (dotimes (y-pos 30)
        (dotimes (x-pos 32)
          (let ((pattern-index (clones.ppu:fetch-nt-byte ppu))
                (frame-index (+ (* y-pos 32) x-pos)))
            (funcall callback pattern-index frame-index)
            (clones.ppu:coarse-scroll-horizontal! ppu)))
        (dotimes (scanline 8)
          (clones.ppu:fine-scroll-vertical! ppu))))))

(defun for-tile-scanlines (bytes callback)
  (loop for index below (/ (length bytes) 2)
        do (let ((low-byte (nth index bytes))
                 (high-byte (nth (+ index 8) bytes)))
             (funcall callback low-byte high-byte))))

(defun draw-tile-to (ppu framebuffer tile x y)
  (let ((tile-bytes (clones.ppu:fetch-tile-bytes ppu tile))
        (high-bits (clones.ppu::palette-high-bits ppu tile)))
    (flet ((draw-tile-line (low-byte high-byte)
             (dotimes (tile-index 8)
               (let* ((low-bits (clones.ppu::palette-low-bits low-byte high-byte tile-index))
                      (palette-index (etypecase tile
                                       (clones.ppu:sprite (+ 16 (dpb high-bits (byte 2 2) low-bits)))
                                       (fixnum (dpb high-bits (byte 2 2) low-bits))))
                      (x-index (+ x tile-index)))
                 (clones.renderer::render-pixel framebuffer ppu y x-index palette-index)))
             (incf y)))
      (for-tile-scanlines tile-bytes #'draw-tile-line))))

(defun coordinates-for (index &key (margin 0) (tile-width 8) (screen-width 256))
  (flet ((layout-fn (index)
           (floor (* index (+ tile-width (* margin 2)))
                  screen-width)))
    (multiple-value-bind (y-pos x-pos) (layout-fn index)
      (values (+ x-pos margin) (* y-pos (+ tile-width margin))))))

(defun dump-graphics (framebuffer ppu &key iterator margin)
  (funcall iterator ppu
           (lambda (tile index)
             (multiple-value-bind (x y) (coordinates-for index :margin margin)
               (draw-tile-to ppu framebuffer tile x y)))))
