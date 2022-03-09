(mgl-pax:define-package :clones
  (:use :cl :alexandria :serapeum :mgl-pax))

(in-package :clones)

(defsection @clones (:title "Clones - An NES Emulator")
  (@links section))

(defsection @links (:title "Links")
  "[![build status][badge]](https://builds.sr.ht/~kingcons/clones/commits/test.yml)
   [badge]: https://builds.sr.ht/~kingcons/clones/commits/test.yml.svg

   Here is the [git repo][repo] and
   here is the [website][site].
   [repo]: https://sr.ht/~kingcons/clones
   [site]: https://clones.kingcons.io")

(defun build-site ()
  (let ((*document-normalize-packages* nil))
    (update-asdf-system-html-docs
     @clones :clones
     :target-dir (asdf:system-relative-pathname :clones "site/")
     :pages `((:objects (,clones:@clones))))))
