.PHONY: all

all: gh-integers my-program

gh-integers: compiler.lisp build.lisp
	sbcl --script ./build.lisp

runtime.o: runtime.c
	gcc -c $^ -o $@

integers.s: ./gh-integers
	./gh-integers $^ > $@

integer.o: ./integers.s
	as -o $@ -c $^

my-program: runtime.o integer.o 
	gcc -o $@ $^

.PHONY: clean
clean: gh-integers my-program runtime.o integers.s integer.o
	rm -rf $^

