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

libClangConfHook (pkg, pbi) flags = do
  lbi <- confHook simpleUserHooks (pkg, pbi) flags

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
               , ldOptions    = ldOptions    libbi ++ ["-Wl,-Bstatic", "-lclang", "-Wl,-Bdynamic"]
               }

  let lib' = lib { libBuildInfo = libbi' }
  let lpd' = lpd { library = Just lib' }

  return $ lbi { localPkgDescr = lpd' }

libClangBuildHook pkg lbi flags ppHandlers = do
    putStrLn "Building llvm and clang..."
    system $ "cd build && make -j8 install"
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
