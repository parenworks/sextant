.PHONY: build clean

build:
	sbcl --non-interactive \
		--eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
		--eval '(push #p"./" asdf:*central-registry*)' \
		--eval '(asdf:load-system :sextant)' \
		--eval '(sb-ext:save-lisp-and-die "sextant" :toplevel #'"'"'sextant:main :executable t :compression t)'

clean:
	rm -f sextant
