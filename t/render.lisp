(in-package :cl-user)

(defpackage :clones-test.render
  (:use :cl :clones.render :prove))

(in-package :clones-test.render)

(plan nil)

(defmacro with-ppu ((var &key nt-bytes at-bytes) &body body)
  `(let ((,var (clones.ppu:make-ppu)))
     (when (and ,nt-bytes ,at-bytes)
       (with-accessors ((nametable clones.ppu:ppu-nametable)) ,var
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
    (is-type *context* 'clones.render:context)))

(defun test-nametables ()
  (subtest "Checking Nametable Access..."
    (diag "With Coarse X of zero...")
    (let ((nt-bytes (coerce (alexandria:iota 32 :start 64) 'vector))
          (at-bytes #(#xAA #xAA #xAA #xAA #xFF #xFF #xFF #xFF)))
      (with-ppu (ppu :nt-bytes nt-bytes :at-bytes at-bytes)
        (is (aref (context-nt-buffer *context*) 0) 0)
        (is (aref (context-at-buffer *context*) 0) 0)
        (render-scanline ppu)
        (is (context-nt-buffer *context*) nt-bytes :test #'equalp)
        (is (context-at-buffer *context*) at-bytes :test #'equalp)))))

(subtest "PPU Rendering ..."
  (test-framebuffer)
  (test-palette)
  (test-context)
  (test-nametables))

(finalize)
