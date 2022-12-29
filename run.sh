#!/bin/bash

dune build

a=
for n in {1..1000};
do
	a+=$RANDOM
done

b=
for n in {1..1000};
do
	b+=$RANDOM
done

hyperfine -r 4 \
	'./_build/default/bench/bench.exe 1 "(Distance Seq "'$a'" "'$b'")"' \
	'./_build/default/bench/bench.exe 2 "(Distance Par "'$a'" "'$b'")"' \
	'./_build/default/bench/bench.exe 4 "(Distance Par "'$a'" "'$b'")"' \
	'./_build/default/bench/bench.exe 8 "(Distance Par "'$a'" "'$b'")"'

#perf stat -d -d -d ./_build/default/bench/bench.exe 1 "(Distance Par "$a" "$b")"
#perf stat -d -d -d ./_build/default/bench/bench.exe 2 "(Distance Par "$a" "$b")"
#perf stat -d -d -d ./_build/default/bench/bench.exe 4 "(Distance Par "$a" "$b")"
