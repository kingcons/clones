(in-package :cl-user)

(defpackage :clones-test.ppu
  (:use :cl :clones.ppu :prove)
  (:import-from :clones.mappers
                :mapper)
  (:import-from :clones.util
                :wrap-palette-table
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
      (is-type (ppu-pattern-table ppu) 'mapper))))

(defun test-cart-swap ()
  (subtest "PPU Cartridge swapping ..."
    (let ((ppu (make-ppu)))
      (is-type (ppu-pattern-table ppu) 'mapper)
      (setf (ppu-pattern-table ppu) nil)
      (is (ppu-pattern-table ppu) nil)
      (setf (ppu-pattern-table ppu) (clones.mappers:load-rom
                                     (clones.util:asset-path "roms/color_test.nes")))
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
      (is (sprite-offset ppu) 0)
      (setf (ppu-control ppu) #b00001000)
      (is (sprite-offset ppu) #x1000)
      (is (background-offset ppu) 0)
      (setf (ppu-control ppu) #b00010000)
      (is (background-offset ppu) #x1000)
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
    (let ((ppu (make-ppu)))
      ;; It sucks to test the implementation here but I'm not
      ;; gonna sidetrack to mock or spy generic functions right now.
      (dotimes (i 4)
        (let ((index (random #x2000)))
          (is (read-vram ppu index)
              (clones.mappers:fetch-chr (ppu-pattern-table ppu) index))))
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
    (is (clones.ppu::nt-mirror :horizontal #x2020) #x20)
    (is (clones.ppu::nt-mirror :horizontal #x2420) #x20)
    (is (clones.ppu::nt-mirror :horizontal #x2820) #x420)
    (is (clones.ppu::nt-mirror :horizontal #x2c20) #x420)
    (is (clones.ppu::nt-mirror :vertical   #x2020) #x20)
    (is (clones.ppu::nt-mirror :vertical   #x2420) #x420)
    (is (clones.ppu::nt-mirror :vertical   #x2820) #x20)
    (is (clones.ppu::nt-mirror :vertical   #x2c20) #x420)))

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
      (is (ppu-write-latch ppu) 0)
      (diag "Reading from PPUDATA returns a buffered value.")
      (setf (ppu-address ppu) #x2020
            (aref (ppu-nametable ppu) #x20) 42)
      (is (ppu-data ppu) 0)
      (is (fetch ppu #x2007) 0)
      (is (ppu-data ppu) 42)
      (diag "Reading the Palette through PPUDATA is unbuffered.")
      (setf (ppu-address ppu) #x3f00
            (aref (ppu-palette-table ppu) 0) 31)
      (is (fetch ppu #x2007) 31)
      ;; TODO: Technically the data should be set to the nametable "underneath" the palette.
      (diag "Reading PPUDATA bumps the PPUADDR by the PPUCTRL step.")
      (setf (ppu-address ppu) #x1020
            (ppu-control ppu) #b00000000)
      (fetch ppu #x2007)
      (is (ppu-address ppu) #x1021)
      (setf (ppu-control ppu) #b00000100)
      (fetch ppu #x2007)
      (is (ppu-address ppu) #x1041))))

(defun test-ppu-write ()
  (subtest "CPU Memory Map - Basic Writes ..."
    (let ((ppu (make-ppu)))
      (store ppu #x2000 #b10001010)
      (is (ppu-control ppu) 138)
      (is (ppu-nt-index ppu) #b10)
      (store ppu #x2001 #b00011110)
      (is (ppu-mask ppu) 30)
      (store ppu #x2002 #b11111111)
      (is (ppu-status ppu) 0)
      (store ppu #x2003 42)
      (is (ppu-oam-address ppu) 42)
      (store ppu #x2003 #xff)
      (store ppu #x2004 42)
      (is (aref (ppu-oam ppu) #xff) 42)
      (is (ppu-oam-address ppu) 0))))

(defun test-ppu-scroll-register ()
  (subtest "PPUSCROLL Writes ..."
    (let ((ppu (make-ppu)))
      (is (ppu-write-latch ppu) 0)
      (store ppu #x2005 #b00111011)
      (is (ppu-write-latch ppu) 1)
      (is (ppu-coarse-x ppu) #b00111)
      (is (ppu-fine-x ppu) #b011)
      (store ppu #x2005 #b11000100)
      (is (ppu-write-latch ppu) 0)
      (is (ppu-coarse-y ppu) #b11000)
      (is (ppu-fine-y ppu) #b100))))

(defun test-ppu-address-register ()
  (subtest "PPUADDR Writes ..."
    (let ((ppu (make-ppu)))
      (is (ppu-write-latch ppu) 0)
      (store ppu #x2006 #b00101010)
      (is (ppu-write-latch ppu) 1)
      (store ppu #x2006 #b00010000)
      (is (ppu-write-latch ppu) 0)
      (is (ppu-address ppu) #b0010101000010000))))

(defun test-write-vram ()
  (subtest "PPU Memory Map - Writes ..."
    (let ((ppu (make-ppu)))
      (setf (ppu-address ppu) #x0020)
      (store ppu #x2007 11)
      (is (read-vram ppu #x0020) 11)
      (is (ppu-address ppu) #x0021)
      (setf (ppu-address ppu) #x2020
            (ppu-control ppu) #b00000100)
      (store ppu #x2007 22)
      (is (read-vram ppu #x2020) 22)
      (is (ppu-address ppu) #x2040)
      (setf (ppu-address ppu) #x3f20)
      (store ppu #x2007 33)
      (is (read-vram ppu #x3f20) 33))))

(defun test-scrolling-tiles ()
  (subtest "PPU Rendering - Coarse X Scrolling"
    (let ((ppu (make-ppu)))
      (is (ppu-coarse-x ppu) 0)
      (is (ppu-nt-index ppu) 0)
      (next-tile ppu 1)
      (is (ppu-coarse-x ppu) 1)
      (next-tile ppu 30)
      (is (ppu-coarse-x ppu) 31)
      (is (ppu-nt-index ppu) 0)
      (next-tile ppu 1)
      (is (ppu-coarse-x ppu) 0)
      (is (ppu-nt-index ppu) 1))))

(defun test-scrolling-lines ()
  (subtest "PPU Rendering - Coarse Y Scrolling"
    (let ((ppu (make-ppu)))
      (is (ppu-coarse-y ppu) 0)
      (next-line ppu)
      (is (ppu-coarse-y ppu) 1)
      (dotimes (i 28)
        (next-line ppu))
      (is (ppu-coarse-y ppu) 29)
      (next-line ppu)
      (is (ppu-coarse-y ppu) 0)
      (is (ppu-nt-index ppu) 2))))

(defun test-nametable-fetch ()
  (subtest "PPU Rendering - Nametable Fetch"
    (let ((ppu (make-ppu)))
      (is (read-nametable ppu) 0)
      (setf (aref (ppu-nametable ppu) 0) #x20
            (aref (ppu-nametable ppu) 1) #x40
            (aref (ppu-nametable ppu) 32) #x80)
      (is (read-nametable ppu) #x20)
      (next-tile ppu 1)
      (is (read-nametable ppu) #x40)
      (next-tile ppu 31)
      ;; NOTE: For a vertically mirrored cartridge the below value would be zero. But
      ;; nestest.rom is horizontally mirrored so wrap around instead of to the next NT.
      (is (read-nametable ppu) #x20)
      (next-line ppu)
      (is (read-nametable ppu) #x80))))

(defun test-attribute-fetch ()
  (subtest "PPU Rendering - Attribute Fetch"
    (let ((ppu (make-ppu)))
      (is (read-attribute ppu) 0)
      (setf (aref (ppu-nametable ppu) #x3c0) #xAA
            (aref (ppu-nametable ppu) #x3c1) #xBB
            (aref (ppu-nametable ppu) #x3c8) #xFF)
      (is (read-attribute ppu) #xAA)
      (next-tile ppu 4)
      (is (read-attribute ppu) #xBB)
      (next-tile ppu 28)
      (next-line ppu)
      (is (read-attribute ppu) #xFF))))

(defun test-pattern-fetch ()
  (subtest "PPU Rendering - Pattern Fetch"
    (let ((ppu (make-ppu)))
      ;; Insert some pattern data to test with since nestest doesn't have any.
      (loop for i below 32
            do (clones.mappers:store-chr (ppu-pattern-table ppu) i (1+ i)))
      (is (read-pattern ppu (background-offset ppu) 0 0 :lo) 1)
      (is (read-pattern ppu (background-offset ppu) 0 0 :hi) 9)
      (is (read-pattern ppu (background-offset ppu) 0 3 :lo) 4)
      (is (read-pattern ppu (background-offset ppu) 0 3 :hi) 12)
      (is (read-pattern ppu (background-offset ppu) 1 0 :lo) 17)
      (is (read-pattern ppu (background-offset ppu) 1 7 :lo) 24))))

(defun test-quad-position ()
  (subtest "PPU Rendering - Color Quad"
    ;; Top Left
    (dolist (coarse-x '(0 1))
      (dolist (coarse-y '(0 1))
        (is (quad-position coarse-x coarse-y) 0)))
    ;; Top Right
    (dolist (coarse-x '(2 3))
      (dolist (coarse-y '(0 1))
        (is (quad-position coarse-x coarse-y) 2)))
    ;; Bottom Left
    (dolist (coarse-x '(0 1))
      (dolist (coarse-y '(2 3))
        (is (quad-position coarse-x coarse-y) 4)))
    ;; Bottom Right
    (dolist (coarse-x '(2 3))
      (dolist (coarse-y '(2 3))
        (is (quad-position coarse-x coarse-y) 6)))))

(defun test-palette-high-bits ()
  (subtest "PPU Rendering - Palette High Bits"
    (let ((attribute-byte #b10001011))
      (is (palette-high-bits attribute-byte 1 1) #b11)
      (is (palette-high-bits attribute-byte 2 1) #b10)
      (is (palette-high-bits attribute-byte 1 2) #b00)
      (is (palette-high-bits attribute-byte 2 2) #b10))))

(defun test-palette-low-bits ()
  (subtest "PPU Rendering - Palette Low Bits"
    (let ((lo-byte #b10101100)
          (hi-byte #b00110011))
      (is (palette-low-bits lo-byte hi-byte 0) 2)
      (is (palette-low-bits lo-byte hi-byte 1) 2)
      (is (palette-low-bits lo-byte hi-byte 2) 1)
      (is (palette-low-bits lo-byte hi-byte 3) 1)
      (is (palette-low-bits lo-byte hi-byte 4) 2)
      (is (palette-low-bits lo-byte hi-byte 5) 3)
      (is (palette-low-bits lo-byte hi-byte 6) 0)
      (is (palette-low-bits lo-byte hi-byte 7) 1))))

(plan nil)

(subtest "PPU Interface"
  (test-ppu-construction)
  (test-control-helpers)
  (test-mask-helpers)
  (test-cart-swap)
  (test-ppu-read)
  (test-read-vram)
  (test-mirroring)
  (test-register-reads)
  (test-ppu-write)
  (test-ppu-scroll-register)
  (test-ppu-address-register)
  (test-write-vram)
  (test-scrolling-tiles)
  (test-scrolling-lines)
  (test-nametable-fetch)
  (test-attribute-fetch)
  (test-pattern-fetch)
  (test-quad-position)
  (test-palette-high-bits)
  (test-palette-low-bits))

(finalize)
