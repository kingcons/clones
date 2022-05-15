(defpackage :clones.test.renderer
  (:use :cl :clones.renderer :try)
  (:export #:test-renderer))

(in-package :clones.test.renderer)

(deftest test-renderer ()
  (test-nmi-disabled)
  (test-nmi-timing)
;  (test-vertical-scroll)
  )

(deftest test-nmi-disabled ()
  (flet ((nmi-handler () (error 'nmi-fired)))
    (destructuring-bind (cpu renderer) (build-renderer :on-nmi #'nmi-handler)
      (clones.cpu:reset cpu)
      (signals-not (error)
        (loop until (= 261 (slot-value renderer 'clones.renderer::scanline))
              do (progn
                   (clones.cpu:single-step cpu)
                   (sync renderer cpu)))))))

(deftest test-nmi-timing ()
  (destructuring-bind (cpu renderer) (build-renderer)
    (catch 'nmi-fired
      (clones.cpu:reset cpu)
      (loop for count = (clones.cpu:single-step cpu)
            do (sync renderer cpu)))
    (is (= (slot-value renderer 'clones.renderer::scanline) 241))))

(defun build-renderer (&key on-nmi)
  (let* ((cpu (clones.cpu:make-cpu))
         (ppu (slot-value (clones.cpu:cpu-memory cpu) 'clones.memory::ppu))
         (nmi-handler (or on-nmi
                          (lambda ()
                            (clones.cpu:nmi cpu)
                            (throw 'nmi-fired nil))))
         (renderer (make-renderer :ppu ppu :on-nmi nmi-handler)))
    (clones.cpu:reset cpu)
    (list cpu renderer)))

(defun test-frame (renderer)
  (with-accessors ((framebuffer renderer-framebuffer)) renderer
    (let ((image (make-instance 'zpng:png
                                :color-type :truecolor
                                :width 512
                                :height 480
                                :image-data (clones.util:scale-2x 512 480 framebuffer))))
      (zpng:write-png image (asdf:system-relative-pathname :clones "test/background.png")))))

#+nil
(try 'test-renderer)
