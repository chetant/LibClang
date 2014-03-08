#!/usr/bin/env bash

CABALTMP=`mktemp LibClang.cabal.XXX`

function exit_hook()
{
    echo reverting backup $CABALTMP to LibClang.cabal, cleanup genfiles
    mv $CABALTMP LibClang.cabal
}

trap exit_hook EXIT

cp LibClang.cabal $CABALTMP && echo copied original LibClang.cabal to $CABALTMP && utils/getExtraSourceFiles.pl -i $CABALTMP -o LibClang.cabal.sdist && perl -lape 's/llvm\/tools\/clang\/test\/PCH\/Inputs\/\*\.h\.gch/llvm\/tools\/clang\/test\/PCH\/Inputs\/badpch-empty.h.gch/' LibClang.cabal.sdist > LibClang.cabal && cabal sdist
