(mgl-pax:define-package :clones.docs
  (:use :cl :alexandria :serapeum :mgl-pax))

(in-package :clones.docs)

(defsection @clones (:title "Clones - An NES Emulator")
  (@links section)
  (@overview section)
  (clones.rom:@rom section)
  (clones.mappers:@mappers section))

(defsection @links (:title "Links")
  "[repo]: https://git.sr.ht/~kingcons/clones
   [site]: https://clones.kingcons.io
   [builds]: https://builds.sr.ht/~kingcons/clones

* [CI Builds][builds]

Here are links to the current [website][site] and [git repo][repo].")

(defsection @overview (:title "Overview")
  "[goals]: https://blog.kingcons.io/posts/Research-Goals.html
   [tgj]: https://twitter.com/leastfixedpoint/status/1026567416229314561

Clones is an early-stage NES emulator written in Common Lisp.

It is inspired by long standing beliefs about the power of computers
for experiential learning. See: [Research Goals][goals].

### Why another emulator?

> Reading things teaches people how to write. Analogous, if we are
to place programming at the same fundamental level, using a
program should teach how it works. But we don't see this.

- Tony Garnock-Jones, [@leastfixedpoint][tgj]

Many NES emulators already exist on the web, the desktop, and elsewhere.
Clones is intended to be a readable, tested, and compact code base sufficient
for mostly accurate emulation of many but not all popular Nintendo titles.

However, my goal is not to be able to relive childhood nostalgia since
that need is thoroughly solved. Clones exists to support curious programmers
learning about how the system and its titles worked rather than being a
vehicle for reliving the past. Running the games is just a prerequisite.

The codebase strives to be accessible for learning about emulation,
sacrificing total accuracy and performance for clarity and ease of modification.
Once some key games (Super Mario Bros, Mega Man 2) are playable, focus will
shift towards building out debugging and reverse engineering tools.

In addition to the usual tools to disassemble memory or view VRAM, we hope to
support building a directed graph of blocks and jumps as games are played.
Afterwards, we'll provide tools for users to visualize and annotate the graph
with notes about the code. This will move us towards our overall goal of making
the high-level structure of programs \"discoverable\" through using them,
calling back to the Tony Garnock-Jones quote above.

### Current Status

Work has just begun so nothing is playable yet. Hang in there.

* ROM Support: ✅
* NROM Mapper: ✅
* CPU Opcodes: ❌
* Rendering - Backgrounds: ❌
* Rendering - Sprites: ❌
* Input Handling: ❌
* MMC1 Mapper: ❌
* Audio Support: ❌")

(defun build-docs ()
  (let ((*document-normalize-packages* nil))
    (update-asdf-system-readmes @clones :clones)))

(defun build-site ()
  (let ((*document-normalize-packages* nil))
    (update-asdf-system-html-docs
     @clones :clones
     :target-dir (asdf:system-relative-pathname :clones "site/")
     :pages `((:objects (,clones.docs:@clones))))))
