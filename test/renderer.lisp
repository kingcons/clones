(defpackage :clones.test.renderer
  (:use :cl :clones.renderer :try)
  (:export #:test-renderer))

(in-package :clones.test.renderer)

(deftest test-renderer ()
  (test-nmi-disabled)
  (test-nmi-timing)
  (test-pattern-pixels))

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

(deftest test-pattern-pixels ()
  (let ((tile-low-byte #b01000001)
        (tile-high-byte #b00000001))
    (is (equalp (clones.renderer::combine-tile-bytes tile-low-byte tile-high-byte)
                #b0001000000000011))))

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

;; (deftest test-pattern-pixels ()
;;   (let ((tile #(#x41 #xC2 #x44 #x48 #x10 #x20 #x40 #x80
;;                 #x01 #x02 #x04 #x08 #x16 #x21 #x42 #x87)))
;;     (is (equalp (tile-pixels tile)
;;                 #(0 1 0 0 0 0 0 3
;;                   1 1 0 0 0 0 3 0
;;                   0 1 0 0 0 3 0 0
;;                   0 1 0 0 3 0 0 0
;;                   0 0 0 3 0 2 2 0
;;                   0 0 3 0 0 0 0 2
;;                   0 3 0 0 0 0 2 0
;;                   3 0 0 0 0 2 2 2)))))

#+nil
(try 'test-renderer)
