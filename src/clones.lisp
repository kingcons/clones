(mgl-pax:define-package :clones
  (:use :cl :alexandria :serapeum :mgl-pax))

(in-package :clones)

(defsection @clones (:title "Clones - An NES Emulator")
  (@links section))

(defsection @links (:title "Links")
  "[repo]: https://sr.ht/~kingcons/clones
   [site]: https://clones.kingcons.io
   [build]: https://builds.sr.ht/~kingcons/clones/commits/.build.yml

   Build Status: [![builds.sr.ht status](https://builds.sr.ht/~kingcons/clones/commits/.build.yml.svg)][build]

   Here are links to the current [git repo][repo] and [website][site].")

(defun build-site ()
  (let ((*document-normalize-packages* nil))
    (update-asdf-system-html-docs
     @clones :clones
     :target-dir (asdf:system-relative-pathname :clones "site/")
     :pages `((:objects (,clones:@clones))))))
