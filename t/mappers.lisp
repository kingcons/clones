(in-package :cl-user)

(defpackage :clones-test.mappers
  (:use :cl :clones.mappers :prove))

(in-package :clones-test.mappers)

(plan 1)

(subtest "NROM Mapper"
  (let* ((nestest (load-rom (clones.util:asset-path "roms/nestest.nes")))
         (aorom-path (clones.util:asset-path "roms/aorom.nes"))
         (invalid-path #p"fake.file"))
    (is-type nestest 'mapper)
    (is-error (load-rom invalid-path) 'file-error)
    (is-error (load-rom aorom-path) 'clones.conditions:unsupported-mapper)
    (is (load-prg nestest 4) 120)
    (is (load-chr nestest 32) 128)
    (is (load-prg nestest 16388) 120)
    (is (mirroring nestest) :horizontal)))

(finalize)

