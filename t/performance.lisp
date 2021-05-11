(in-package :cl-user)

(defpackage :clones-test.performance
  (:use :cl :prove)
  (:import-from :alexandria
   :symbolicate
   :read-file-into-byte-vector)
  (:import-from :clones.memory
   :fetch
   :store)
  (:import-from :clones.util
   :asset-path))

(in-package :clones-test.performance)

(defmacro with-patched-functions (bindings &body body)
  (let* ((gensyms (loop for (name) in bindings collect (gensym)))
         (backups (loop for gensym in gensyms
                        for (name definition) in bindings
                        collect `(,gensym (fdefinition ',name))))
         (patches (loop for (name definition) in bindings
                        append `((fdefinition ',name) ,definition)))
         (reverts (loop for (name) in bindings
                        for gensym in gensyms
                        append `((fdefinition ',name) ,gensym))))
    `(let ,backups
       (setf ,@patches)
       ,@body
       (setf ,@reverts))))

(defun maybe-init-opcodes ()
  (let ((initializer (find-symbol "INIT-OPCODE-INFO" :clones.instruction-data)))
    (when initializer
      (funcall initializer))))

(defun klaus-init (cpu ram rom)
  (loop for byte across rom
        for i = 10 then (1+ i)
        do (setf (aref ram i) byte))
  (setf (clones.cpu:cpu-pc cpu) #x1000))

(defun speedrun ()
  (maybe-init-opcodes)
  (let ((cpu (clones.cpu:make-cpu)))
    (setf (clones.cpu:cpu-pc cpu) #xC000)
    (time (loop until (= (clones.cpu:cpu-pc cpu) #xc6bd)
                do (clones.instructions:single-step cpu)))))

(defun klaus-speedrun ()
  (maybe-init-opcodes)
  (let ((cpu (clones.cpu:make-cpu))
        (ram (make-array #xffff :element-type '(unsigned-byte 8)))
        (rom (read-file-into-byte-vector (asset-path "roms/test.bin"))))
    (klaus-init cpu ram rom)
    (with-patched-functions ((fetch (lambda (memory address)
                                      (declare (ignore memory))
                                      (aref ram address)))
                             (store (lambda (memory address value)
                                      (declare (ignore memory))
                                      (setf (aref ram address) value))))
      (time (loop until (= (clones.cpu:cpu-pc cpu) #x3c37)
                  do (clones.instructions:single-step cpu))))))
