.PHONY: all

all: 
	make -C ./chapters/01-integers
	make -C ./chapters/02-unary-primitives
	make -C ./chapters/03-binary-primitives

.PHONY: clean
clean:
	make clean -C ./chapters/01-integers
	make clean -C ./chapters/02-unary-primitives
	make clean -C ./chapters/03-binary-primitives
