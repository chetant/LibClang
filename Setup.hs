{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

import Control.Exception
import Control.Monad
import Control.Applicative
import Data.List (isPrefixOf)
import Distribution.PackageDescription
import Distribution.Verbosity
import Distribution.Simple
#if MIN_VERSION_Cabal(1,24,0)
import Distribution.Simple.BuildPaths hiding (mkLibName)
#endif
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Find
import System.Posix.Files(createSymbolicLink)
import System.Directory
import System.FilePath
import System.Info(os)

main :: IO ()
main = do
  curDir <- getCurrentDirectory

  let confProgram' = confProgram
        { programFindLocation =  \v _ -> findProgramOnSearchPath v
                                         [ProgramSearchPathDir $ curDir </> "llvm"] "configure"
        }

  defaultMainWithHooks simpleUserHooks
      { confHook  = libClangConfHook
      , buildHook = libClangBuildHook
      , copyHook  = libClangCopyHook
      , cleanHook = libClangCleanHook

      , hookedPrograms = hookedPrograms simpleUserHooks ++
                         [ confProgram'
                         , libtoolProgram
                         , makeProgram
                         , swVersProgram
                         ]
      }

libclangLibraries :: [String]
libclangLibraries =
  [ "clang_static"
  , "clangARCMigrate"
  , "clangAST"
  , "clangAnalysis"
  , "clangBasic"
  , "clangCodeGen"
  , "clangDriver"
  , "clangEdit"
  , "clangFormat"
  , "clangFrontend"
  , "clangFrontendTool"
  , "clangIndex"
  , "clangLex"
  , "clangParse"
  , "clangRewriteCore"
  , "clangRewriteFrontend"
  , "clangSema"
  , "clangSerialization"
  , "clangTooling"
  , "LLVMBitReader"
  , "LLVMCore"
  , "LLVMMCParser"
  , "LLVMMC"
  , "LLVMOption"
  , "LLVMSupport"
  , "LLVMTransformUtils"
  ]

libclangSharedLibraries :: [String]
libclangSharedLibraries = ["clang", "LLVM-3.4"]

libClangConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags
                 -> IO LocalBuildInfo
libClangConfHook (pkg, pbi) flags = do
  let verbosity = fromFlag (configVerbosity flags)
  lbi <- confHook simpleUserHooks (pkg, pbi) flags

  -- Compute some paths that need to be absolute.
  curDir <- getCurrentDirectory
  let llvmRepoDir   = curDir </> "llvm"
      llvmBuildDir  = curDir </> "build"
      llvmPrefixDir = llvmBuildDir </> "out"
      clangRepoDir  = curDir </> "clang"
      clangLinkPath = llvmRepoDir </> "tools" </> "clang"

  -- Infer which standard library to use.
  cppStdLib <- preferredStdLib (withPrograms lbi) flags
  let (linkCPPStdLib, confCPPStdLib) =
        case cppStdLib of
          LibStdCPP -> ("-lstdc++", "no")
          LibCPP    -> ("-lc++", "yes")

  let addLdOptions = onLocalLibBuildInfo
                   . onLdOptions
                   $ (++ [ "-lpthread"
                         , linkCPPStdLib
                         , "-lncurses"
                         ])

  -- Ensure that the LLVM build process sees clang.
  createDirectoryIfMissingVerbose verbosity True llvmPrefixDir
  clangLinkExists <- doesDirectoryExist clangLinkPath
  unless clangLinkExists $ createSymbolicLink clangRepoDir clangLinkPath

  notice verbosity "Configuring LLVM and Clang..."

  makefileExists <- doesFileExist $ llvmBuildDir </> "Makefile"
  unless makefileExists $
    inDir llvmBuildDir $
      runDbProgram verbosity confProgram (withPrograms lbi)
             [ "--enable-optimized"
             , "--enable-keep-symbols"
             , "--disable-jit"
             , "--disable-docs"
             , "--enable-shared"
             , "--enable-bindings=none"
             , "--enable-libcpp=" ++ confCPPStdLib
             , "--prefix=" ++ llvmPrefixDir
             ]

  return $ addLdOptions lbi

libClangBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
libClangBuildHook pkg lbi usrHooks flags = do
    curDir <- getCurrentDirectory
    let verbosity = fromFlag (buildVerbosity flags)
        llvmPrefixDir = curDir </> "build" </> "out"
        llvmLibDir = llvmPrefixDir </> "lib"
        libclangLibrariesFiles = map mkStaticLib libclangLibraries

        addLLVMComponents = onIncludeDirs (++ [".", llvmPrefixDir </> "include"])
                          . onExtraLibs (++ libclangSharedLibraries)
                          . onExtraLibDirs (++ [llvmLibDir])

        addGHCArgs = onProgram ghcProgram
                   . onProgramOverrideArgs
                   $ (++ ["-optl-Wl,-rpath," ++
                          libdir (absoluteInstallDirs pkg lbi NoCopyDest)])

        addHsc2hsArgs = if os == "darwin"
                          then onProgram hsc2hsProgram
                             . onProgramOverrideArgs
                             $ (++ [ "--lflag=-Xlinker"
                                   , "--lflag=-rpath"
                                   , "--lflag=-Xlinker"
                                   , "--lflag=" ++ llvmLibDir
                                   ])
                          else id

        lbi' = onLocalLibBuildInfo addLLVMComponents
             . onPrograms (addGHCArgs . addHsc2hsArgs)
             $ lbi

    notice verbosity "Building LLVM and Clang..."

    -- FIXME: We'd ideally like to use the -j option given to cabal-install itself.
    -- Alternatively we could use a command-specific option like
    -- 'cabal install --make-option=-j4', but see
    -- https://github.com/haskell/cabal/issues/1380 for why this doesn't work.
    -- For now we hardcore "-j4".
    inDir (curDir </> "build") $
      runDbProgram verbosity makeProgram (withPrograms lbi') ["-j4", "install"]

    -- OS X's linker _really_ wants to link dynamically, and it doesn't support
    -- the options you'd usually use to control that on Linux. We rename the
    -- libclang library to make sure the linker does what we intend.
    copyFileVerbose verbosity (llvmLibDir </> mkStaticLib "clang")
                              (llvmLibDir </> mkStaticLib "clang_static")

    -- Build with the wrappers generated by C2HS.
    buildHook
      simpleUserHooks
      (onLibrary
       (\l -> onLibBuildInfo
          (\bi -> onCSources
             (\srcs -> ["dist/build/Clang/Internal/FFI.chs.c"] ++ srcs)
             bi )
          l)
       (localPkgDescr lbi'))
      lbi'
      usrHooks
      flags

    notice verbosity "Relinking..."

    let componentLibs = concatMap componentLibNames $ componentsConfigs lbi'
        libclangLibs = map (llvmLibDir </>) libclangLibrariesFiles
        mergeLibs = mergeLibraries verbosity lbi' libclangLibs

    -- Merge the libclang static libraries into each Haskell static library.
    forM_ componentLibs $ \componentLib -> do
      when (withVanillaLib lbi') $
#if MIN_VERSION_Cabal(1,24,0)
        mergeLibs $ mkLibName (getHSLibraryName componentLib)
#else
        mergeLibs $ mkLibName componentLib
#endif
      when (withProfLib lbi') $
        mergeLibs $ mkProfLibName componentLib

mergeLibraries :: Verbosity -> LocalBuildInfo -> [FilePath] -> FilePath -> IO ()
mergeLibraries verbosity lbi libclangLibs libName = do
  let libtool = runLibtool verbosity lbi
      ar = runAr verbosity lbi
      bdir = buildDir lbi
      finalLib = bdir </> libName
      origLib = bdir </> libName <.> "orig"
      allLibs = origLib : libclangLibs

  copyFileVerbose verbosity finalLib origLib

  if os == "darwin"
     then libtool $ ["-static", "-o", finalLib] ++ allLibs
     else ar ["-M"] $ arScript finalLib allLibs

arScript :: FilePath -> [FilePath] -> String
arScript finalLib allLibs = unlines $ ["CREATE " ++ finalLib]
                                   ++ map ("ADDLIB " ++) allLibs
                                   ++ ["SAVE", "END"]

libClangCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
libClangCopyHook pkg lbi hooks flags = do
  copyHook simpleUserHooks pkg lbi hooks flags

  curDir <- getCurrentDirectory
  let llvmLibDir = curDir </> "build" </> "out" </> "lib"
      verbosity = fromFlag (copyVerbosity flags)
      destdir = fromFlagOrDefault NoCopyDest $ copyDest flags
      libCopyDir = libdir $ absoluteInstallDirs pkg lbi destdir

  notice verbosity $ "Installing libclang shared libraries (" ++ show libCopyDir ++ ")..."
  copyFiles verbosity libCopyDir $ map ((llvmLibDir,) . mkSharedLib) libclangSharedLibraries

libClangCleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
libClangCleanHook pkg v hooks flags = do
  curDir <- getCurrentDirectory
  let verbosity = fromFlag (cleanVerbosity flags)
      buildDir = curDir </> "build"

  notice verbosity "Cleaning LLVM and Clang..."

  buildDirExists <- doesDirectoryExist buildDir
  when buildDirExists $ removeDirectoryRecursive buildDir

  cleanHook simpleUserHooks pkg v hooks flags

runLibtool :: Verbosity -> LocalBuildInfo -> [String] -> IO ()
runLibtool verbosity lbi args = do
  output <- getDbProgramOutput verbosity libtoolProgram (withPrograms lbi) args
  when (verbosity >= deafening) $
    putStrLn output

runAr :: Verbosity -> LocalBuildInfo -> [String] -> String -> IO ()
runAr v lbi args script =
    case lookupProgram arProgram (withPrograms lbi) of
      Nothing    -> die "Couldn't find required program 'ar'"
      Just cProg -> runProgramInvocation v (progWithStdin cProg)
  where
    progWithStdin prog = (programInvocation prog args') { progInvokeInput = Just script }
    args' = if v >= deafening then "-v" : args else args

#if MIN_VERSION_Cabal(1,24,0)
mkLibName :: String -> String
mkLibName lname =
  "lib" ++ lname <.> "a"
#endif

mkStaticLib :: String -> String
#if MIN_VERSION_Cabal(1,24,0)
mkStaticLib lname = mkLibName lname
#else
mkStaticLib lname = mkLibName (LibraryName lname)
#endif

mkSharedLib :: String -> String
mkSharedLib lname = "lib" ++ lname <.> dllExtension

#if MIN_VERSION_Cabal(1,24,0)
componentLibNames :: (ComponentName, ComponentLocalBuildInfo, [ComponentName]) -> [UnitId]
componentLibNames (_, LibComponentLocalBuildInfo {..}, _) = [componentUnitId]
#else
componentLibNames :: (ComponentName, ComponentLocalBuildInfo, [ComponentName]) -> [LibraryName]
componentLibNames (_, LibComponentLocalBuildInfo {..}, _) = componentLibraries
#endif
componentLibNames _                                       = []

confProgram, libtoolProgram, makeProgram, swVersProgram :: Program
confProgram    = simpleProgram "configure"
libtoolProgram = simpleProgram "libtool"
makeProgram    = simpleProgram "make"
swVersProgram  = simpleProgram "sw_vers"

inDir :: FilePath -> IO a -> IO a
inDir dir act = do
  curDir <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir)
           (setCurrentDirectory curDir)
           act

data CPPStdLib = LibStdCPP
               | LibCPP
                 deriving (Eq, Show)

preferredStdLib :: ProgramDb -> ConfigFlags -> IO CPPStdLib
preferredStdLib pdb flags =
  case (preferLibStdCPP, preferLibCPP, os) of
    (Just True, _, _)     -> return LibStdCPP
    (_, Just True, _)     -> return LibCPP
    (_, _, "darwin")      -> darwinPreferredStdLib pdb flags
    _                     -> return LibStdCPP
  where
    preferLibCPP    = lookup (FlagName "preferlibcpp") $ configConfigurationsFlags flags
    preferLibStdCPP = lookup (FlagName "preferlibstdcpp") $ configConfigurationsFlags flags

darwinPreferredStdLib :: ProgramDb -> ConfigFlags -> IO CPPStdLib
darwinPreferredStdLib pdb flags = do
  let verbosity = fromFlag (configVerbosity flags)
  darwinVer <- getDbProgramOutput verbosity swVersProgram pdb ["-productVersion"]

  -- We want LibStdCPP for 10.8 and below, but LibCPP for anything newer.
  let libStdCPPVers = ["10.0", "10.1", "10.2", "10.3", "10.4", "10.5", "10.6", "10.7", "10.8"]
  return $ if any (`isPrefixOf` darwinVer) libStdCPPVers
             then LibStdCPP
             else LibCPP

type Lifter a b = (a -> a) -> b -> b

onLocalPkgDescr :: Lifter PackageDescription LocalBuildInfo
onLocalPkgDescr f lbi = lbi { localPkgDescr = f (localPkgDescr lbi) }

onPrograms :: Lifter ProgramDb LocalBuildInfo
onPrograms f lbi = lbi { withPrograms = f (withPrograms lbi) }

onLibrary :: Lifter Library PackageDescription
onLibrary f lpd = lpd { library = f <$> library lpd }

onLibBuildInfo :: Lifter BuildInfo Library
onLibBuildInfo f lib = lib { libBuildInfo = f (libBuildInfo lib) }

onCSources :: Lifter [FilePath] BuildInfo
onCSources f libbi = libbi { cSources = f (cSources libbi) }

onLocalLibBuildInfo :: Lifter BuildInfo LocalBuildInfo
onLocalLibBuildInfo = onLocalPkgDescr . onLibrary . onLibBuildInfo

onIncludeDirs :: Lifter [FilePath] BuildInfo
onIncludeDirs f libbi = libbi { includeDirs = f (includeDirs libbi) }

onExtraLibs :: Lifter [FilePath] BuildInfo
onExtraLibs f libbi = libbi { extraLibs = f (extraLibs libbi) }

onExtraLibDirs :: Lifter [FilePath] BuildInfo
onExtraLibDirs f libbi = libbi { extraLibDirs = f (extraLibDirs libbi) }

onLdOptions :: Lifter [FilePath] BuildInfo
onLdOptions f libbi = libbi { ldOptions = f (ldOptions libbi) }

onProgram :: Program -> Lifter ConfiguredProgram ProgramDb
onProgram prog f pdb = case lookupProgram prog pdb of
                         Just cProg -> updateProgram (f cProg) pdb
                         Nothing    -> pdb

onProgramOverrideArgs :: Lifter [String] ConfiguredProgram
onProgramOverrideArgs f prog = prog { programOverrideArgs = f (programOverrideArgs prog) }
