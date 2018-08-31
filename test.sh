sbcl --eval "(ql:quickload :clones-test)" --eval "(prove:run \"t/ppu.lisp\" :reporter :list)" --eval "(sb-ext:quit)"
# sbcl --eval "(asdf:test-system :clones)" --eval "(sb-ext:quit)"
