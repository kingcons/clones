(in-package :cl-user)

(defpackage :clones-test.render
  (:use :cl :clones.render :prove))

(in-package :clones-test.render)

(plan nil)

(defmacro with-ppu ((var &key nt-bytes at-bytes) &body body)
  `(let ((,var (clones.ppu:make-ppu)))
     (when (and ,nt-bytes ,at-bytes)
       (with-accessors ((nametable clones.ppu::ppu-nametable)) ,var
         (setf (subseq nametable 0 (length ,nt-bytes)) ,nt-bytes
               (subseq nametable #x3c0 (+ #x3c0 (length ,at-bytes))) ,at-bytes)))
     ,@body))

(defun test-framebuffer ()
  (subtest "Checking framebuffer..."
    (is-type (subseq *framebuffer* 0 #x10) 'clones.util:byte-vector)))

(defun test-palette ()
  (subtest "Checking palette..."
    (is-type (subseq +color-palette+ 0 #x10) 'clones.util:byte-vector)))

(defun test-context ()
  (subtest "Checking render context..."
    (is-type *context* 'clones.render:context)
    (is (context-scanline *context*) 0)
    (is (context-dma-p *context*) nil)
    (is (context-nmi-p *context*) nil)
    (is (context-frame-p *context*) nil)
    (is (length (context-nt-buffer *context*)) 32)
    (is (length (context-at-buffer *context*)) 8)
    (is (length (context-candidates *context*)) 8)))

(subtest "PPU Rendering ..."
  (test-framebuffer)
  (test-palette)
  (test-context))

(finalize)
