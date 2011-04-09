

all: testclang

test_diagnostics: test_diagnostics.c
	gcc -std=gnu99 -I/home/saiko/bin/llvm/include/ -L/home/saiko/bin/llvm/lib/ -lclang -lLLVM-3.0svn -lpthread -ldl -lm -o test_diagnostics test_diagnostics.c

clean:
	rm test_diagnostics

.PHONY = all clean
