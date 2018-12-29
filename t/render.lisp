(in-package :cl-user)

(defpackage :clones-test.render
  (:use :cl :clones.render :prove))

(in-package :clones-test.render)

(plan nil)

(defun test-framebuffer ()
  (subtest "Checking framebuffer..."
    (is-type (subseq *framebuffer* 0 #x10) 'clones.util:byte-vector)))

(defun test-palette ()
  (subtest "Checking palette..."
    (is-type (subseq +color-palette+ 0 #x10) 'clones.util:byte-vector)))

(subtest "PPU Rendering ..."
  (test-framebuffer)
  (test-palette))

(finalize)
