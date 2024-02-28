.PHONY: all

all: 
	make -C ./chapters/integers
	make -C ./chapters/unary-primitives

.PHONY: clean
clean:
	make clean -C ./chapters/integers
	make clean -C ./chapters/unary-primitives
