(in-package :cl-user)

(defpackage :clones-test.memory
  (:use :cl :clones.memory :prove))

(in-package :clones-test.memory)

(plan 2)

(subtest "Storing and Fetching Data from RAM"
   (let* ((memory (make-memory)))
     (is (fetch memory 0) 0)
     (store memory 0 32)
     (store memory #x7ff 31)
     (is (fetch memory 0) 32)
     (is (fetch memory #x7ff) 31)
     (is (fetch-range memory #x7ff #x801) '(31 32 0))
     (subtest "The same values are mirrored every 2K"
       (is (fetch memory #x800) 32)
       (is (fetch memory #xfff) 31))))

(subtest "Storing and Fetching Data from Mapper (NROM)"
  (let* ((memory (make-memory)))
    (is (fetch memory #x8000) 76)
    (is (fetch memory #xffff) 197)))

(finalize)
