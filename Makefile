day := 03

peft: build-perf
	perf record -F97 --call-graph dwarf -- ./day$(day)

build-perf: 
	roc build --optimize --profiling --linker=legacy day$(day).roc
