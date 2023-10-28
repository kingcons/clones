.PHONY: all app docs repl site test clean

all: app docs test

clean:
	rm -rf bin/clones site

app:
	sbcl --eval "(ql:quickload :clones)" \
	     --eval "(asdf:make :clones)" \
	     --quit

docs:
	sbcl --non-interactive \
	     --eval "(ql:quickload :clones)" \
	     --eval "(funcall #'clones:build-docs)" \
	     --quit

repl:
	rm -f .slynk-port
	sbcl --eval "(ql:quickload :clones)" \
	     --eval "(clones:main)"

site:
	sbcl --non-interactive \
	     --eval "(ql:quickload :clones)" \
	     --eval "(funcall #'clones.docs:build-site)" \
	     --quit

test:
	sbcl --non-interactive \
	     --eval "(ql:quickload :clones/test)" \
	     --eval "(funcall #'clones.test:test-ci)" \
	     --quit
