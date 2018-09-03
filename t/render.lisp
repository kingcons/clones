(in-package :cl-user)

(defpackage :clones-test.render
  (:use :cl :clones.render :prove))

(in-package :clones-test.render)

(plan nil)

(subtest "PPU Rendering ..."
  (is-type *framebuffer* 'clones.util:byte-vector)
  (is-type +color-palette+ 'clones.util:byte-vector))

(finalize)
