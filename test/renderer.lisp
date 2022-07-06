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
    (let ((framebuffer (clones::make-framebuffer)))
      (destructuring-bind (cpu renderer) (build-renderer :on-nmi #'nmi-handler)
        (signals-not (error)
          (loop for cycles = (clones.cpu:single-step cpu)
                for result = (sync renderer cpu framebuffer)
                until (eql result 261)))))))

(deftest test-nmi-timing ()
  (destructuring-bind (cpu renderer) (build-renderer)
    (let ((framebuffer (clones::make-framebuffer)))
      (catch 'nmi-fired
        (loop for count = (clones.cpu:single-step cpu)
              do (sync renderer cpu framebuffer))))
    (is (= (slot-value renderer 'clones.renderer::scanline) 241))))

(deftest test-nametable-rendering ()
  (let* ((ppu (clones.ppu:make-ppu))
         (framebuffer (clones::make-framebuffer))
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
    (clones.debug:dump-graphics framebuffer ppu :iterator #'clones.debug:for-background)
    (let ((image (make-image framebuffer)))
      (zpng:write-png image test-image-path))
    (let ((test-dir (asdf:system-relative-pathname :clones "test/")))
      (uiop:with-current-directory (test-dir)
        (is (null (uiop:run-program "diff dk-background.png nametable.png")))))
    (uiop:delete-file-if-exists test-image-path)))

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

#+nil
(try 'test-renderer)
