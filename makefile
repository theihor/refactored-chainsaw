all:
	sbcl --load build.lisp "$@"
clean:
	rm -f main
	rm -rf traces/generated

run-problems: clean all

package: 
