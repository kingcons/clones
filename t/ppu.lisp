(in-package :cl-user)

(defpackage :clones-test.ppu
  (:use :cl :clones.ppu :prove)
  (:import-from :clones.mappers
                :load-rom
                :mapper)
  (:import-from :clones.util
                :wrap-palette-table
                :asset-path
                :ub8))

(in-package :clones-test.ppu)

(defun test-ppu-construction ()
  (subtest "PPU Structure ..."
    (let ((ppu (make-ppu)))
      (is (type-of ppu) 'ppu)
      (is (ppu-control ppu) 0)
      (is (ppu-mask ppu) 0)
      (is (ppu-status ppu) 0)
      (is (ppu-oam-address ppu) 0)
      (is (ppu-address ppu) 0)
      (is (ppu-data ppu) 0)
      (is (ppu-coarse-x ppu) 0)
      (is (ppu-coarse-y ppu) 0)
      (is (ppu-fine-x ppu) 0)
      (is (ppu-fine-y ppu) 0)
      (is (ppu-nt-index ppu) 0)
      (is (ppu-write-latch ppu) 0)
      (is (length (ppu-oam ppu)) #x100)
      (is (length (ppu-nametable ppu)) #x800)
      (is (length (ppu-palette-table ppu)) #x20)
      (is (ppu-pattern-table ppu) nil))))

(defun test-cart-swap ()
  (subtest "PPU Cartridge swapping ..."
    (let ((ppu (make-ppu)))
      (is (ppu-pattern-table ppu) nil)
      (setf (ppu-pattern-table ppu) (load-rom (asset-path "roms/color_test.nes")))
      (is-type (ppu-pattern-table ppu) 'mapper))))

(defun test-control-helpers ()
  (subtest "PPUCTRL bit readers ..."
    (let ((ppu (make-ppu)))
      (is (x-scroll-offset ppu) 0)
      (setf (ppu-control ppu) #b00000001)
      (is (x-scroll-offset ppu) 256)
      (is (y-scroll-offset ppu) 0)
      (setf (ppu-control ppu) #b00000010)
      (is (y-scroll-offset ppu) 240)
      (is (vram-step ppu) 1)
      (setf (ppu-control ppu) #b00000100)
      (is (vram-step ppu) 32)
      (is (sprite-base-address ppu) 0)
      (setf (ppu-control ppu) #b00001000)
      (is (sprite-base-address ppu) #x1000)
      (is (background-base-address ppu) 0)
      (setf (ppu-control ppu) #b00010000)
      (is (background-base-address ppu) #x1000)
      (is (sprite-size ppu) 8)
      (setf (ppu-control ppu) #b00100000)
      (is (sprite-size ppu) 16)
      (is (vblank-p ppu) nil)
      (setf (ppu-control ppu) #b10000000)
      (is (vblank-p ppu) t))))

(defun test-mask-helpers ()
  (subtest "PPUMASK bit readers ..."
    (let ((ppu (make-ppu)))
      (is (grayscale-p ppu) nil)
      (setf (ppu-mask ppu) #b00000001)
      (is (grayscale-p ppu) t)
      (is (show-background-left-p ppu) nil)
      (setf (ppu-mask ppu) #b00000010)
      (is (show-background-left-p ppu) t)
      (is (show-sprites-left-p ppu) nil)
      (setf (ppu-mask ppu) #b00000100)
      (is (show-sprites-left-p ppu) t)
      (is (show-background-p ppu) nil)
      (setf (ppu-mask ppu) #b00001000)
      (is (show-background-p ppu) t)
      (is (show-sprites-p ppu) nil)
      (setf (ppu-mask ppu) #b00010000)
      (is (show-sprites-p ppu) t)
      (is (emphasize-red-p ppu) nil)
      (setf (ppu-mask ppu) #b00100000)
      (is (emphasize-red-p ppu) t)
      (is (emphasize-green-p ppu) nil)
      (setf (ppu-mask ppu) #b01000000)
      (is (emphasize-green-p ppu) t)
      (is (emphasize-blue-p ppu) nil)
      (setf (ppu-mask ppu) #b10000000)
      (is (emphasize-blue-p ppu) t))))

(defun test-ppu-read ()
  (subtest "CPU Memory Map - Reads ..."
    (let ((ppu (make-ppu))
          (invalid-reads '(#x2000 #x2001 #x2003 #x2005 #x2006)))
      (dolist (addr invalid-reads)
        (is (fetch ppu addr) 0))
      (setf (ppu-data ppu) 31
            (ppu-status ppu) 42
            (ppu-oam-address ppu) 53
            (aref (ppu-oam ppu) 53) 64)
      (is (fetch ppu #x2002) 42)
      (is (fetch ppu #x2004) 64)
      (is (fetch ppu #x2007) 31))))

(defun test-read-vram ()
  (subtest "PPU Memory Map - Reads ..."
    (let ((ppu (make-ppu))
          (rom (load-rom (asset-path "roms/color_test.nes"))))
      (setf (ppu-pattern-table ppu) rom)
      ;; It sucks to test the implementation here but I'm not
      ;; gonna sidetrack to mock or spy generic functions right now.
      (dotimes (i 4)
        (let ((index (random #x2000)))
          (is (read-vram ppu index)
              (clones.mappers:fetch-chr rom index))))
      (setf (aref (ppu-nametable ppu) #x20) 42
            (aref (ppu-nametable ppu) #x420) 27)
      (is (read-vram ppu #x2020) 42)
      (is (read-vram ppu #x2420) 42)
      (is (read-vram ppu #x2820) 27)
      (is (read-vram ppu #x2c20) 27)
      (dotimes (i 4)
        (let ((index (random #xff))
              (color (random #xff)))
          (setf (aref (ppu-palette-table ppu) (wrap-palette-table index)) color)
          (is (read-vram ppu (+ #x3f00 index)) color))))))

(defun test-mirroring ()
  (subtest "Nametable Mirroring ..."
    (is (clones.ppu::nt-offset :horizontal #x2020) 0)
    (is (clones.ppu::nt-offset :horizontal #x2420) 0)
    (is (clones.ppu::nt-offset :horizontal #x2820) #x400)
    (is (clones.ppu::nt-offset :horizontal #x2c20) #x400)
    (is (clones.ppu::nt-offset :vertical   #x2020) 0)
    (is (clones.ppu::nt-offset :vertical   #x2420) #x400)
    (is (clones.ppu::nt-offset :vertical   #x2820) 0)
    (is (clones.ppu::nt-offset :vertical   #x2c20) #x400)))

(defun test-register-reads ()
  (subtest "PPU Register Read Behavior ..."
    (let ((ppu (make-ppu)))
      (diag "Reading from PPUSTATUS clears the vblank bit.")
      (setf (ppu-status ppu) #b10001010)
      (fetch ppu #x2002)
      (is (ppu-status ppu) #b00001010)
      (diag "Reading from PPUSTATUS resets the write latch.")
      (setf (ppu-write-latch ppu) 1)
      (is (ppu-write-latch ppu) 1)
      (fetch ppu #x2002)
      (is (ppu-write-latch ppu) 0))))

(plan nil)

(subtest "PPU Interface"
  (test-ppu-construction)
  (test-control-helpers)
  (test-mask-helpers)
  (test-cart-swap)
  (test-ppu-read)
  (test-read-vram)
  (test-mirroring)
  (test-register-reads))

(finalize)
