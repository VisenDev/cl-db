LISP?=sbcl

main:
	$(LISP) --eval '(load "init.lisp")' \
	--eval '(asdf:make "open-orders")' \
	--eval '(uiop:quit)'

clean:
	find . -type f -name '*.fasl' -exec trash {} \;
	if [ -e ./open-orders ]; then trash open-orders; fi

