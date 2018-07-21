all: clean
	sbcl --load build.lisp "$@"
clean:
	rm -f main

ts := $(shell /bin/date "+%Y_%m_%d_%H_%M_%S")

package: 
	echo $(ts)
	cd ./best_traces && zip --password 935f74a4701a420388faa8c809f3af2c ../TheWildLobsters_$(ts).zip *.*
	shasum -a 256 TheWildLobsters_$(ts).zip
