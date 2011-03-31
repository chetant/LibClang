

all: testclang

testclang: testclang.c
	gcc -std=gnu99 -I/home/saiko/bin/llvm/include/ -L/home/saiko/bin/llvm/lib/ -lclang -lLLVM-3.0svn -lpthread -ldl -lm -o testclang testclang.c

clean:
	rm testclang

.PHONY = all clean
