.PHONY: all app docs site test clean

all: app docs test

app:
	sbcl --eval "(ql:quickload :clones)" \
	     --eval "(asdf:make :clones)" \
	     --quit

dev:
	sbcl --eval "(ql:quickload :clones)" \
	     --eval "(clones:main)" \
	     --quit

docs:
	sbcl --non-interactive \
	     --eval "(ql:quickload :clones)" \
	     --eval "(funcall #'clones:build-docs)" \
	     --quit

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

clean:
	rm -rf bin/clones site
