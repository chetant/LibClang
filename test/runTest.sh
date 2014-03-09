#!/bin/sh

TEST=$1
CTEST=${TEST}_c
HSTEST=${TEST}_hs

shift

TMPFILE=`mktemp -t libclangtest_XXX`
TMPFILE_C=$TMPFILE.ref
TMPFILE_HS=$TMPFILE.test
./$CTEST $* | grep -v "'linker' input unused" > $TMPFILE_C
./$HSTEST $* | grep -v "'linker' input unused" > $TMPFILE_HS
if [ -n "`diff $TMPFILE_C $TMPFILE_HS`" ]
then
  echo FAILED: $TEST refOutput: $TMPFILE_C  testOutput: $TMPFILE_HS
else
  echo SUCCESS: $TEST
  rm $TMPFILE_HS $TMPFILE_C
fi
