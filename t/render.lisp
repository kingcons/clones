(in-package :cl-user)

(defpackage :clones-test.render
  (:use :cl :clones.render :prove))

(in-package :clones-test.render)

(plan nil)

(subtest "PPU Rendering ..."
  (is-type (subseq *framebuffer* 0 #x10) 'clones.util:byte-vector)
  (is-type (subseq +color-palette+ 0 #x10) 'clones.util:byte-vector))

(finalize)
