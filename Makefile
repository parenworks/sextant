.PHONY: build test clean

build:
	sbcl --non-interactive \
		--eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
		--eval '(push #p"./" asdf:*central-registry*)' \
		--eval '(ql:quickload :sextant)' \
		--eval '(sb-ext:save-lisp-and-die "sextant" :toplevel #'"'"'sextant:main :executable t :compression t)'

test:
	sbcl --non-interactive \
		--eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
		--eval '(push #p"./" asdf:*central-registry*)' \
		--eval '(asdf:test-system :sextant)'

clean:
	rm -f sextant
