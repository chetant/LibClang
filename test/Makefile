CC=gcc
CFLAGS=-std=gnu99 -I/home/saiko/bin/llvm/include/
LDFLAGS=-L/home/saiko/bin/llvm/lib/ -lclang -lLLVM-3.0svn -lpthread -ldl -lm

all: Test_Diagnostics_c Test_Diagnostics_hs Test_Inclusionvisitor_c Test_Inclusionvisitor_hs

%_c : %.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

%_hs : %.hs
	ghc -o $@ $<

clean:
	rm *_c *_hs *.o *.hi

.PHONY = all clean
