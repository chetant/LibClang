import Data.Maybe (fromJust)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Install
import Distribution.Simple.Setup
import Distribution.Simple.Utils (currentDir)
import System.Cmd (system)
import System.Directory
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
         { confHook = libClangConfHook
         , cleanHook = libClangCleanHook
         , buildHook = libClangBuildHook
         , postInst = myPostInst
         }

libclangLibraries :: [String]
libclangLibraries =
  [ "-lclang_static"
  , "-lclangARCMigrate"
  , "-lclangAST"
  , "-lclangAnalysis"
  , "-lclangBasic"
  , "-lclangCodeGen"
  , "-lclangDriver"
  , "-lclangEdit"
  , "-lclangFormat"
  , "-lclangFrontend"
  , "-lclangFrontendTool"
  , "-lclangLex"
  , "-lclangParse"
  , "-lclangRewriteCore"
  , "-lclangRewriteFrontend"
  , "-lclangSema"
  , "-lclangSerialization"
  , "-lclangTooling"
  , "-lLLVMSupport"
  , "-lLLVMMCParser"
  , "-lLLVMMC"
  , "-lLLVMBitReader"
  ]

libClangConfHook (pkg, pbi) flags = do
  lbi <- confHook simpleUserHooks (pkg, pbi) flags

  putStrLn "Ensuring the build directory exists..."
  system $ "mkdir -p build"

  putStrLn "Linking clang repository into llvm repository..."
  system $ "ln -sf `pwd`/clang llvm/tools/clang"

  putStrLn "Configuring llvm and clang..."
  system $ "cd build && test -e Makefile || ../llvm/configure"
        ++ " --enable-optimized"
        ++ " --disable-clang-rewriter"
        ++ " --disable-clang-static-analyzer"
        ++ " --disable-clang-arcmt"
        ++ " --enable-keep-symbols"
        ++ " --disable-jit"
        ++ " --disable-docs"
        ++ " --enable-bindings=none"
        ++ " --disable-pic"
        ++ " --disable-shared"
        ++ " --prefix=`pwd`/out"

  let lpd   = localPkgDescr lbi
  let lib   = fromJust (library lpd)
  let libbi = libBuildInfo lib

  let curDir = "/Users/mfowler/Code/LibClang"
  let libbi' = libbi
               { extraLibDirs = extraLibDirs libbi ++ [curDir ++ "/build/out/lib"]
               --, extraLibs    = extraLibs    libbi ++ ["clang", "pthread"]
               , includeDirs  = includeDirs  libbi ++ [curDir ++ "/build/out/include"]
               , ccOptions    = ccOptions    libbi ++ ["-I.", "-I" ++ curDir ++ "/build/out/include"]
               , ldOptions    = ldOptions    libbi ++ libclangLibraries
               }

  let lib' = lib { libBuildInfo = libbi' }
  let lpd' = lpd { library = Just lib' }

  return $ lbi { localPkgDescr = lpd' }

libClangBuildHook pkg lbi flags ppHandlers = do
    putStrLn "Building llvm and clang..."
    system $ "cd build && make -j8 install"

    -- OS X's linker _really_ wants to link dynamically, and it doesn't support
    -- the options you'd usually use to control that on Linux. We rename the
    -- libclang library to make sure the linker does what we intend.
    putStrLn "Ensuring libclang is available as libclang_static..."
    system $ "cd build/out/lib && test -e libclang.a && mv libclang.a libclang_static.a"

    (buildHook simpleUserHooks) pkg lbi flags ppHandlers

libClangCleanHook pkg v hooks flags = do
    putStrLn "Cleaning llvm and clang..."
    --system $ "cd build && rm -rf *"
    (cleanHook simpleUserHooks) pkg v hooks flags

myPostInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostInst _ _ pkgdesc lbi = do
  let dirs = absoluteInstallDirs pkgdesc lbi NoCopyDest
  libExecHook dirs

-- hook to move helper binaries to the libexec directory
libExecHook :: InstallDirs String -> IO ()
libExecHook dirs = do
  let bdir = bindir dirs
  let ldir = libdir dirs
  createDirectoryIfMissing True ldir
  putStrLn $ "lDir=" ++ ldir
  renameFile "/Users/mfowler/Code/LibClang/build/out/lib/libclang.dylib" (ldir </> "libclang.dylib")
