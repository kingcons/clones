(defpackage :clones.test.renderer
  (:use :cl :clones.renderer :try)
  (:export #:test-renderer))

(in-package :clones.test.renderer)

(deftest test-renderer ()
  (test-nmi-disabled)
  (test-nmi-timing)
  (test-nametable-rendering))

(deftest test-nmi-disabled ()
  (flet ((nmi-handler () (error 'nmi-fired)))
    (destructuring-bind (cpu renderer) (build-renderer :on-nmi #'nmi-handler)
      (signals-not (error)
        (loop for cycles = (clones.cpu:single-step cpu)
              for result = (sync renderer cpu)
              until (eql result 261))))))

(deftest test-nmi-timing ()
  (destructuring-bind (cpu renderer) (build-renderer)
    (catch 'nmi-fired
      (loop for count = (clones.cpu:single-step cpu)
            do (sync renderer cpu)))
    (is (= (slot-value renderer 'clones.renderer::scanline) 241))))

(deftest test-nametable-rendering ()
  (let* ((ppu (clones.ppu:make-ppu))
         (name-table-path (asdf:system-relative-pathname :clones "test/dk-nametable.json"))
         (pattern-table-path (asdf:system-relative-pathname :clones "test/dk-pattern.json"))
         (test-image-path (asdf:system-relative-pathname :clones "test/nametable.png")))
    (with-open-file (in name-table-path)
      (setf (slot-value ppu 'clones.ppu::name-table) (shasht:read-json in)))
    (with-open-file (in pattern-table-path)
      (let ((nrom (slot-value ppu 'clones.ppu::pattern-table)))
        (setf (slot-value nrom 'clones.mappers::chr) (shasht:read-json in))))
    (setf (slot-value ppu 'clones.ppu::palette) #(15 44 56 18 15 39 39 39 15 48 48 48 15
                                                  0 0 0 0 37 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (setf (ldb (byte 1 4) (clones.ppu::ppu-ctrl ppu)) 1)
    (let ((image (make-image (render-nametable ppu 0))))
      (zpng:write-png image test-image-path))
    (let ((test-dir (asdf:system-relative-pathname :clones "test/")))
      (uiop:with-current-directory (test-dir)
        (is (null (uiop:run-program "diff dk-background.png nametable.png")))))
    (uiop:delete-file-if-exists test-image-path)))

(defun render-nametable (ppu nt-index)
  "Render the nametable of the PPU selected by NT-INDEX.
Scroll information is not taken into account."
  (let ((framebuffer (clones.renderer::make-framebuffer))
        (scanline-buffer (serapeum:make-octet-vector 256))
        (return-address (clones.ppu::ppu-address ppu))
        (nt-address (* #x400 nt-index)))
    ;; TODO: The base address for a nametable should be offset by #x2000 right?
    ;; If viewing the nametable bytes yes, but the PPUADDR is set to the pattern
    ;; bytes to draw, not the nametable address. Setting low bits in PPUCTRL may
    ;; be the right approach here. Need to test with other ROMs/nametables.
    (setf (clones.ppu::ppu-address ppu) nt-address)
    (dotimes (scanline 240)
      (dotimes (tile 32)
        (clones.renderer::render-tile ppu scanline-buffer (clones.ppu:fetch-nt-byte ppu))
        (clones.ppu:coarse-scroll-horizontal! ppu))
      (dotimes (pixel 256)
        (let ((value (aref scanline-buffer pixel)))
          (clones.renderer::render-pixel framebuffer ppu scanline pixel value)))
      (clones.ppu:fine-scroll-vertical! ppu))
    (setf (clones.ppu::ppu-address ppu) return-address)
    framebuffer))

(defun build-renderer (&key on-nmi)
  (let* ((cpu (clones.cpu:make-cpu))
         (ppu (slot-value (clones.cpu:cpu-memory cpu) 'clones.memory::ppu))
         (nmi-handler (or on-nmi
                          (lambda ()
                            (clones.cpu:nmi cpu)
                            (throw 'nmi-fired nil))))
         (renderer (make-renderer :ppu ppu :on-nmi nmi-handler :on-frame (constantly nil))))
    (clones.cpu:reset cpu)
    (list cpu renderer)))

(defun make-image (data &key (width 256) (height 240))
  (make-instance 'zpng:png
                 :color-type :truecolor
                 :width width
                 :height height
                 :image-data data))

(defun test-frame (renderer)
  (declare (ignore renderer))
  (let* ((data (clones.util:scale-2x 512 480 *framebuffer*))
         (image (make-image data :width 512 :height 480)))
    (zpng:write-png image (asdf:system-relative-pathname :clones "test/debug.png"))))

#+nil
(try 'test-renderer)
