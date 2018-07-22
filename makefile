all: clean
	sbcl --load build.lisp "$@"

build-best-traces: clean
	sbcl --load build-get-best-traces.lisp "$@"

clean:
	rm -f main
	rm -f get-best-traces

best-traces: build-best-traces
	./get-best-traces 

ts := $(shell /bin/date "+%Y_%m_%d_%H_%M_%S")

package: 
	echo $(ts)
	cd ./best_traces && zip --password 935f74a4701a420388faa8c809f3af2c ../TheWildLobsters_$(ts).zip *.*
	shasum -a 256 TheWildLobsters_$(ts).zip
