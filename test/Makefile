CC=gcc
CFLAGS=-std=gnu99 -I/home/saiko/bin/llvm/include/
LDFLAGS=-L/home/saiko/bin/llvm/lib/ -lclang -lLLVM-3.0svn -lpthread -ldl -lm

all: Test_Diagnostics_c Test_Diagnostics_hs Test_InclusionVisitor_c Test_InclusionVisitor_hs

test: all
	@echo "########### TESTING ##############"
	@./runTest.sh Test_InclusionVisitor $(CFLAGS) Test_InclusionVisitor.c
	@./runTest.sh Test_Diagnostics $(CFLAGS) Test_Diagnostics.c

%_c : %.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

%_hs : %.hs
	ghc -o $@ $<

clean:
	rm *_c *_hs *.o *.hi

.PHONY = all clean test
