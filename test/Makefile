CC=gcc
CFLAGS=-std=gnu99

LLVM_CFLAGS=$(shell $(LLVM_CONFIG) --cflags)
LDFLAGS=$(shell $(LLVM_CONFIG) --ldflags)
LIBS=-ldl -pthread -Wl,-rpath -Wl,$(shell $(LLVM_CONFIG) --libdir) $(shell $(LLVM_CONFIG) --libs core) -lclang
all: Test_Diagnostics_c Test_Diagnostics_hs Test_InclusionVisitor_c Test_InclusionVisitor_hs Test_ChildVisitor_c Test_ChildVisitor_hs

test: all
	@echo "########### TESTING ##############"
	@./runTest.sh Test_Diagnostics Test_Diagnostics.c
	@./runTest.sh Test_InclusionVisitor Test_InclusionVisitor.c
	@./runTest.sh Test_ChildVisitor Test_ChildVisitor.c

%_c : %.c
	$(CC) $(CFLAGS) $(LLVM_CFLAGS) -o $@ $< $(LDFLAGS) $(LIBS)

%_hs : %.hs
	cd .. && cabal exec -- ghc -o test/$@ test/$<

clean:
	rm *_c *_hs *.o *.hi

.PHONY = all clean test
