.PHONY: test run clean

run:
	sbcl --load brain.lisp

test:
	sbcl --script test_runner.lisp

clean:
	rm -rf ~/.cache/common-lisp/
	rm -f *.fasl
