import Data.Maybe (fromJust)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Install
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.System
import Distribution.Verbosity (Verbosity)
import System.Cmd (system)
import System.Directory
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
         { confHook = libClangConfHook
         , cleanHook = libClangCleanHook
         , buildHook = libClangBuildHook
         , hookedPreProcessors = [("fficonst", ffiConstHook)]
         }

ffiConstHook :: BuildInfo -> LocalBuildInfo -> PreProcessor
ffiConstHook _ _ =
  PreProcessor
  { platformIndependent = True
  , runPreProcessor = mkSimplePreProcessor runFFIConstPass
  }

runFFIConstPass :: FilePath -> FilePath -> Verbosity -> IO ()
runFFIConstPass inFile outFile _ = do
  putStrLn "Building FFI constant preprocessor..."
  curDir <- getCurrentDirectory
  let libclangIncludeDir = curDir </> "clang" </> "include"
      cbitsIncludeDir    = curDir </> "cbits"
  system $ "cd fficonst && make CPPFLAGS="
        ++ escape ("-I" ++ libclangIncludeDir ++ " -I" ++ cbitsIncludeDir)

  putStrLn "Generating FFI constants..."
  system $ "cp -f " ++ escape inFile ++ " " ++ escape outFile
  system $ "./fficonst/fficonst >> " ++ escape outFile

  return ()

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
  , "-lLLVMCore"
  ]

libClangConfHook (pkg, pbi) flags = do
  lbi <- confHook simpleUserHooks (pkg, pbi) flags

  -- Compute some paths that need to be absolute.
  curDir <- getCurrentDirectory
  let clangRepoDir = curDir </> "clang"
  let llvmPrefixDir = curDir </> "build" </> "out"

  putStrLn "Ensuring the build directory exists..."
  system $ "mkdir -p build"

  putStrLn "Linking clang repository into llvm repository..."
  system $ "ln -sf " ++ (escape clangRepoDir) ++ " llvm/tools/clang"

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
        ++ " --disable-shared"
        ++ " --prefix=" ++ (escape llvmPrefixDir)

  let lpd   = localPkgDescr lbi
  let lib   = fromJust (library lpd)
  let libbi = libBuildInfo lib

  -- On OS X the default compiler is clang and the default C++ standard library
  -- is libc++. Ironically given that this is a wrapper for libclang, we run
  -- into trouble with libc++. As a workaround, on that platform we request
  -- libstdc++ explicitly. This could be handled more cleanly but I think the
  -- best longterm solution is to debug the issue with libc++.
  let stdlibArg = case buildOS of
                    OSX -> ["-stdlib=libstdc++"]
                    _   -> []

  let libbi' = libbi
               { extraLibDirs = extraLibDirs libbi ++ [llvmPrefixDir </> "lib"]
               , includeDirs  = includeDirs  libbi ++ [".", llvmPrefixDir </> "include"]
               , ldOptions    = stdlibArg          ++
                                ["-lpthread"]      ++
                                libclangLibraries  ++
                                ["-lstdc++"]       ++
                                ldOptions    libbi ++
                                libclangLibraries  ++
                                ["-lstdc++"]
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
    system $ "rm -rf build/*"

    putStrLn "Cleaning the FFI constant preprocessor..."
    system $ "rm -f fficonst/fficonst"

    (cleanHook simpleUserHooks) pkg v hooks flags

escape :: FilePath -> String
escape p = "\"" ++ p ++ "\""
