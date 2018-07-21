all: clean
	sbcl --load build.lisp "$@"
clean:
	rm -f main

package: 
