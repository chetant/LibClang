AC_INIT([LibClang], [0.0.1])
AC_CONFIG_SRCDIR([src/Clang.hs])

AC_ARG_WITH([llvm-config], [AS_HELP_STRING([--with-llvm-config],[configure LLVM libraries with llvm-config @<:@default: yes@:>@])], [llvm_config=${withval}], [llvm_config=yes])
AC_CHECK_PROG([llvm_config_found], [llvm-config], [yes], [no])
if test "$llvm_config" = yes
then
	if test "$llvm_config_found" = no
	then
		AC_MSG_ERROR([
	-------------------------------------------------
	llvm-config not found.
	if llvm-config is present, ensure it is available
	on path (export PATH=...).
	NOTE: if file is present and configured,
	try running running "cabal build -vv" or 
	           "runhaskell Setup.hs build -vv"
	to get the exact commands run
	-------------------------------------------------])
	else
		# get relevant libs from llvm-config
		CFLAGS="$CFLAGS `llvm-config --cflags`"
		LDFLAGS="$LDFLAGS `llvm-config --ldflags`"
	fi
fi

AC_ARG_ENABLE([llvm-shared],[AS_HELP_STRING([--enable-llvm-shared=LIBSUFFIX],[build against LLVM shared libraries with libsuffix])],[llvm_shared=$enableval])
if test "$llvm_shared" != ""
then
	AC_CHECK_LIB([${llvm_shared}], [LLVMContextCreate], [llvm_shared_lib_found=yes], [llvm_shared_lib_found=no])
	if test "$llvm_shared_lib_found" = no
	then
		AC_MSG_ERROR([
	-------------------------------------------------
	llvm shared library ${llvm_shared} not found.
	if the library is present, ensure it is available
	on the lib search path (export LDFLAGS=...)
	-------------------------------------------------])
	else
		LDFLAGS="$LDFLAGS -l${llvm_shared}"
		AC_TRY_RUN([int main(int argc, int argv[]){LLVMContextCreate();return 0;}], [llvm_shared_lib_present=yes], [llvm_shared_lib_present=no])
		if test "$llvm_shared_lib_present" = no
		then
		AC_MSG_ERROR([
	-------------------------------------------------------
	specified llvm shared library ${llvm_shared} not found.
	if the library is present, ensure it is available
	on the lib search path (export LID_LIBRARY_PATH=...)
	-------------------------------------------------------])
		fi
	fi
else
	if test "$llvm_config_found" = yes
	then
		LDFLAGS="$LDFLAGS `llvm-config --libs`"
	fi
fi

AC_CHECK_HEADERS([clang-c/Index.h], [], [no_clang_include=yes])
if test "$no_clang_include" = yes
then
	AC_MSG_ERROR([
	-------------------------------------------------
	Cannot find <clang-c/Index.h>
	Please ensure libClang is installed and 
	is in CFLAGS (-I...) or passed through configure
	NOTE: if file is present and configured,
	try running running "cabal build -vv" or 
	           "runhaskell Setup.hs build -vv"
	to get the exact commands run
	-------------------------------------------------])
fi

AC_CHECK_LIB([clang], [clang_getClangVersion], [clang_lib_found=yes], [clang_lib_found=no])
if test "$clang_lib_found" = no
then
	AC_MSG_ERROR([
-------------------------------------------------
clang library not found.
if the library is present, ensure it is available
on the lib search path (export LDFLAGS=...)
-------------------------------------------------])
else
	LDFLAGS="$LDFLAGS -lclang"
fi

# Check for stdlib.h and inttypes
if test "$ac_cv_header_stdlib_h" != yes
then
	AC_MSG_ERROR([Cannot find stdlib.h])
fi
if test "$ac_cv_header_inttypes_h" != yes
then
	AC_MSG_ERROR([Cannot find inttypes.h])
fi

AC_CONFIG_FILES([LibClang.buildinfo])
AC_OUTPUT
