(defpackage :clones.test.renderer
  (:use :cl :clones.renderer :try)
  (:export #:test-renderer))

(in-package :clones.test.renderer)

(deftest test-renderer ()
  (test-pattern-pixels))

(deftest test-pattern-pixels ()
  (let ((tile-low-byte #b01000001)
        (tile-high-byte #b00000001))
    (is (equalp (clones.renderer::combine-tile-bytes tile-low-byte tile-high-byte)
                #b0001000000000011))))

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
(try 'test-render)
