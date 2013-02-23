#!/bin/sh

TEST=$1
CTEST=${TEST}_c
HSTEST=${TEST}_hs

TMPFILE_C=`mktemp`
TMPFILE_HS=`mktemp`
./$CTEST $* | grep -v "'linker' input unused" > $TMPFILE_C
./$HSTEST $* | grep -v "'linker' input unused" > $TMPFILE_HS
if [ -n "`diff $TMPFILE_C $TMPFILE_HS`" ]
then
  echo FAILED: $TEST refOutput: $TMPFILE_C  testOutput: $TMPFILE_HS
else
  echo SUCCESS: $TEST
  rm $TMPFILE_HS $TMPFILE_C
fi
