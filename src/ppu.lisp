(in-package :cl-user)

(defpackage :clones.ppu
  (:use :cl)
  (:import-from :alexandria :define-constant))

(in-package :clones.ppu)

(define-constant *color-palette*
  #(#x7C #x7C #x7C #x00 #x00 #xFC #x00 #x00 #xBC #x44 #x28 #xBC
    #x94 #x00 #x84 #xA8 #x00 #x20 #xA8 #x10 #x00 #x88 #x14 #x00
    #x50 #x30 #x00 #x00 #x78 #x00 #x00 #x68 #x00 #x00 #x58 #x00
    #x00 #x40 #x58 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #xBC #xBC #xBC #x00 #x78 #xF8 #x00 #x58 #xF8 #x68 #x44 #xFC
    #xD8 #x00 #xCC #xE4 #x00 #x58 #xF8 #x38 #x00 #xE4 #x5C #x10
    #xAC #x7C #x00 #x00 #xB8 #x00 #x00 #xA8 #x00 #x00 #xA8 #x44
    #x00 #x88 #x88 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #xF8 #xF8 #xF8 #x3C #xBC #xFC #x68 #x88 #xFC #x98 #x78 #xF8
    #xF8 #x78 #xF8 #xF8 #x58 #x98 #xF8 #x78 #x58 #xFC #xA0 #x44
    #xF8 #xB8 #x00 #xB8 #xF8 #x18 #x58 #xD8 #x54 #x58 #xF8 #x98
    #x00 #xE8 #xD8 #x78 #x78 #x78 #x00 #x00 #x00 #x00 #x00 #x00
    #xFC #xFC #xFC #xA4 #xE4 #xFC #xB8 #xB8 #xF8 #xD8 #xB8 #xF8
    #xF8 #xB8 #xF8 #xF8 #xA4 #xC0 #xF0 #xD0 #xB0 #xFC #xE0 #xA8
    #xF8 #xD8 #x78 #xD8 #xF8 #x78 #xB8 #xF8 #xB8 #xB8 #xF8 #xD8
    #x00 #xFC #xFC #xF8 #xD8 #xF8 #x00 #x00 #x00 #x00 #x00 #x00)
  :documentation "The color palette used by the graphics card." :test #'equalp)
