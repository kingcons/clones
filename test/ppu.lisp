(defpackage :clones.test.ppu
  (:use :cl :clones.ppu :try)
  (:export #:test-ppu))

(in-package :clones.test.ppu)

(deftest test-ppu ()
  (test-registers))

(deftest test-registers ()
  (let ((ppu (make-ppu)))
    (test-ctrl-register ppu)
    (test-mask-register ppu)
    (test-oam-data ppu)
    (test-write-latch ppu)
    (test-ppu-scroll ppu)
    (test-ppu-address ppu)
    (test-ppu-data ppu)))

(deftest test-ctrl-register (ppu)
  (test-nametable-address ppu)
  (test-vram-increment ppu)
  (test-sprite-address ppu)
  (test-background-address ppu)
  (test-sprite-size ppu)
  (test-vblank-nmi? ppu))

(deftest test-nametable-address (ppu)
  (is (= (clones.ppu::nametable-address ppu) #x2000))
  (write-ppu ppu 0 1)
  (is (= (clones.ppu::nametable-address ppu) #x2400))
  (write-ppu ppu 0 2)
  (is (= (clones.ppu::nametable-address ppu) #x2800))
  (write-ppu ppu 0 3)
  (is (= (clones.ppu::nametable-address ppu) #x2C00))
  (write-ppu ppu 0 0))

(deftest test-vram-increment (ppu)
  (is (= (clones.ppu::vram-increment ppu) 1))
  (write-ppu ppu 0 4)
  (is (= (clones.ppu::vram-increment ppu) 32))
  (write-ppu ppu 0 0))

(deftest test-sprite-address (ppu)
  (is (= (clones.ppu::sprite-address ppu) 0))
  (write-ppu ppu 0 8)
  (is (= (clones.ppu::sprite-address ppu) #x1000))
  (write-ppu ppu 0 0))

(deftest test-background-address (ppu)
  (is (= (clones.ppu::background-address ppu) 0))
  (write-ppu ppu 0 16)
  (is (= (clones.ppu::background-address ppu) #x1000))
  (write-ppu ppu 0 0))

(deftest test-sprite-size (ppu)
  (is (eql (clones.ppu::sprite-size ppu) :8x8))
  (write-ppu ppu 0 32)
  (is (eql (clones.ppu::sprite-size ppu) :8x16))
  (write-ppu ppu 0 0))

(deftest test-vblank-nmi? (ppu)
  (is (eql (clones.ppu::vblank-nmi? ppu) nil))
  (write-ppu ppu 0 128)
  (is (eql (clones.ppu::vblank-nmi? ppu) t))
  (write-ppu ppu 0 0))

(deftest test-mask-register (ppu)
  (flet ((mask-helper (index function)
           (is (eql (funcall function ppu) nil))
           (write-ppu ppu 1 (ash 1 index))
           (is (eql (funcall function ppu) t))
           (write-ppu ppu 1 0)))
    (mask-helper 0 'clones.ppu::grayscale?)
    (mask-helper 1 'clones.ppu::show-background-left?)
    (mask-helper 2 'clones.ppu::show-sprite-left?)
    (mask-helper 3 'clones.ppu::show-background?)
    (mask-helper 4 'clones.ppu::show-sprite?)))

(deftest test-oam-data (ppu)
  (write-ppu ppu 4 42)
  (is (not (= (read-ppu ppu 4) 42)))
  (write-ppu ppu 3 0)
  (is (= (read-ppu ppu 4) 42))
  (write-ppu ppu 3 0))

(deftest test-write-latch (ppu)
  ;; TODO test ppustatus reads resetting the write latch
  )

(deftest test-ppu-scroll (ppu)
  (test-scrolling-ctrl-register ppu)
  (test-scrolling-scroll-register ppu))

(deftest test-scrolling-ctrl-register (ppu)
  (is (= (clones.ppu::ppu-scroll ppu) 0))
  (write-ppu ppu 0 3)
  (is (= (ldb (byte 2 10) (clones.ppu::ppu-scroll ppu)) 3))
  (write-ppu ppu 0 0))

(deftest test-scrolling-scroll-register (ppu)
  ;; TODO: Can we rewrite this to test the actual bit ranges and be more clear?
  ;; It is based on the bit diagrams in docs/ppu/scrolling.txt
  (is (= (clones.ppu::ppu-scroll ppu) 0))
  (is (eql (clones.ppu::ppu-write-latch ppu) nil))
  (write-ppu ppu 5 170) ;; #b10101010
  (is (= (clones.ppu::ppu-fine-x ppu) 2))
  (is (= (clones.ppu::ppu-scroll ppu) 21))
  (is (eql (clones.ppu::ppu-write-latch ppu) t))
  (write-ppu ppu 5 85) ;; #b01010101
  (is (= (clones.ppu::ppu-scroll ppu) 20821))
  (is (eql (clones.ppu::ppu-write-latch ppu) nil)))

(deftest test-ppu-address (ppu)
  (is (eql (clones.ppu::ppu-write-latch ppu) nil))
  (write-ppu ppu 6 #x21) ;; #b00100001
  (is (= (ldb (byte 6 8) (clones.ppu::ppu-scroll ppu))
         (ldb (byte 6 0) #x21)))
  (is (null (logbitp 14 (clones.ppu::ppu-scroll ppu))))
  (is (eql (clones.ppu::ppu-write-latch ppu) t))
  (write-ppu ppu 6 #x08) ;; #b00001000
  (is (= (ldb (byte 8 0) (clones.ppu::ppu-scroll ppu))
         #x08))
  (is (= (clones.ppu::ppu-address ppu)
         (clones.ppu::ppu-scroll ppu)))
  (is (eql (clones.ppu::ppu-write-latch ppu) nil)))

(deftest test-ppu-data (ppu)
  (test-vram-access ppu)
  (test-vram-buffer ppu))

(deftest test-vram-access (ppu)
  (setf (clones.ppu::ppu-address ppu) #x3535)
  (is (= (clones.ppu::vram-increment ppu) 1))
  (is (= (read-ppu ppu 7) 0))
  (is (= (clones.ppu::ppu-address ppu) #x3536))
  (write-ppu ppu 7 #xBB)
  (is (= (clones.ppu::ppu-address ppu) #x3537))
  (setf (clones.ppu::ppu-address ppu) #x3536)
  (read-ppu ppu 7)
  (is (= (clones.ppu::ppu-address ppu) #x3537)))

(deftest test-vram-buffer (ppu)
  (test-regular-vram-buffer ppu)
  (test-palette-vram-buffer ppu))

(deftest test-regular-vram-buffer (ppu)
  (setf (clones.ppu::ppu-address ppu) #x3636)
  (write-ppu ppu 7 42)
  (setf (clones.ppu::ppu-address ppu) #x3636)
  (read-ppu ppu 7)
  (is (= (read-ppu ppu 7) 42)))

(deftest test-palette-vram-buffer (ppu)
  (setf (clones.ppu::ppu-address ppu) #x3F10)
  (write-ppu ppu 7 25)
  (setf (clones.ppu::ppu-address ppu) #x3F10)
  (is (= (read-ppu ppu 7) 25)))

#+nil
(try 'test-ppu)
