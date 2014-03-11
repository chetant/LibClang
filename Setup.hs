import Data.Maybe (fromJust)
import Control.Monad
import Control.Applicative
import Distribution.PackageDescription
import Distribution.Verbosity
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Install
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.Simple.Program.Find
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Run
import Distribution.Simple.Program.Ar(createArLibArchive)
import qualified Distribution.ModuleName as ModuleName
import System.Posix.Files(createSymbolicLink)
import System.Directory
import System.FilePath
import System.Info(os)

main :: IO ()
main = do
  curDir <- getCurrentDirectory
  let cfgPrg = (simpleProgram "configure") { programFindLocation = findCfgPrg }
      findCfgPrg v _ = findProgramOnSearchPath v
                       [ProgramSearchPathDir $ curDir </> "llvm"] "configure"
  defaultMainWithHooks simpleUserHooks
      { confHook = libClangConfHook
      , cleanHook = libClangCleanHook
      , buildHook = libClangBuildHook
      , instHook = libClangInstallHook

      , hookedPrograms = hookedPrograms simpleUserHooks ++
                         [ simpleProgram "make"
                         , cfgPrg
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

libClangConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
libClangConfHook (pkg, pbi) flags = do
  let verbosity = fromFlag (configVerbosity flags)
  lbi <- confHook simpleUserHooks (pkg, pbi) flags

  -- Compute some paths that need to be absolute.
  curDir <- getCurrentDirectory
  let clangRepoDir  = curDir </> "clang"
      llvmRepoDir   = curDir </> "llvm"
      llvmBuildDir  = curDir </> "build"
      llvmPrefixDir = llvmBuildDir </> "out"

      lpd    = localPkgDescr lbi
      lib    = fromJust (library lpd)
      libbi  = libBuildInfo lib
      libbi' = libbi { ldOptions = ldOptions libbi ++ ["-lpthread", "-lstdc++"] }
      lib'   = lib { libBuildInfo = libbi' }
      lpd'   = lpd { library = Just lib' }
      pdb  = withPrograms lbi 
      confPrg = maybe (error "'configure' not found!") id (lookupKnownProgram "configure" pdb)

  createDirectoryIfMissingVerbose verbosity True llvmPrefixDir
  let clangLinkPath = llvmRepoDir </> "tools" </> "clang"
  clangLinkExists <- doesDirectoryExist clangLinkPath
  when (not clangLinkExists) $ createSymbolicLink clangRepoDir clangLinkPath

  notice verbosity "Configuring llvm and clang..."
  (cfgCmd, _) <- requireProgram verbosity confPrg pdb
  makefileExists <- doesFileExist $ llvmBuildDir </> "Makefile"
  when (not makefileExists) $ do
    setCurrentDirectory llvmBuildDir
    runProgram verbosity cfgCmd
           [ "--enable-optimized"
           , "--enable-keep-symbols"
           , "--disable-jit"
           , "--disable-docs"
           , "--enable-shared"
           , "--enable-bindings=none"
           , "--prefix=" ++ llvmPrefixDir
           ]
  setCurrentDirectory curDir

  return $ lbi { localPkgDescr = lpd' }

libClangBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
libClangBuildHook pkg lbi usrHooks flags = do
    curDir <- getCurrentDirectory
    let verbosity = fromFlag (buildVerbosity flags)
        clangRepoDir = curDir </> "clang"
        llvmPrefixDir = curDir </> "build" </> "out"
        llvmPrefixTmpDir = llvmPrefixDir </> "tmp"
        libclangLibrariesFiles = map mkStaticLib libclangLibraries

        -- setup local build info with clang and llvm provided as libs
        lpd   = localPkgDescr lbi
        lib   = fromJust (library lpd)
        libbi = libBuildInfo lib

        libbi' = libbi
                 { extraLibDirs = extraLibDirs libbi ++ [llvmPrefixDir </> "lib"]
                 , extraLibs    = extraLibs    libbi ++ libclangSharedLibraries
                 , includeDirs  = includeDirs  libbi ++ [".", llvmPrefixDir </> "include"]
                 }

        lib' = lib { libBuildInfo = libbi' }
        lpd' = lpd { library = Just lib' }
        pdb  = withPrograms lbi
        Just ghcPrg = lookupProgram ghcProgram pdb
        ghcPrg' = ghcPrg { programOverrideArgs = programOverrideArgs ghcPrg ++ 
                                                 ["-optl-Wl,-rpath," ++ 
                                                  (libdir $ absoluteInstallDirs pkg lbi NoCopyDest)
                                                 ]
                         }
        Just hsc2hsPrg = lookupProgram hsc2hsProgram pdb
        hsc2hsPrg' = hsc2hsPrg { programOverrideArgs = programOverrideArgs hsc2hsPrg ++
                                                       ["--lflag=-Xlinker"
                                                       ,"--lflag=-rpath"
                                                       ,"--lflag=-Xlinker"
                                                       ,"--lflag=@executable_path/../../../../build/out/lib"
                                                       ]
                               }
        makeProg = maybe (error "'make' not found!") id (lookupKnownProgram "make" pdb)
        pdb' = foldr updateProgram pdb [ghcPrg'
                                       ,if os == "darwin" then hsc2hsPrg' else hsc2hsPrg]
        lbi' = lbi { localPkgDescr = lpd', withPrograms = pdb' }
        bdir = buildDir lbi'

    notice verbosity "Building llvm and clang..."
    (makeCmd, _) <- requireProgram verbosity makeProg pdb
    setCurrentDirectory (curDir </> "build")
    -- FIXME: We hardcode -j4 here because of a bug in cabal that wont let us pass cmd specific flags
    --        would ideally like to use the -j option given to cabal-install itself
    --        alternatively use the cmd specific option: 'cabal install --make-option=-j4', 
    --        but see https://github.com/haskell/cabal/issues/1380 for why this doesn't work
    runProgram verbosity makeCmd ["-j4", "install"]
    setCurrentDirectory curDir

    -- OS X's linker _really_ wants to link dynamically, and it doesn't support
    -- the options you'd usually use to control that on Linux. We rename the
    -- libclang library to make sure the linker does what we intend.
    copyFileVerbose verbosity (llvmPrefixDir </> "lib" </> mkStaticLib "clang") 
                              (llvmPrefixDir </> "lib" </> mkStaticLib "clang_static")

    -- build the library with the new lbi and pkgDesc
    (buildHook simpleUserHooks) lpd' lbi' usrHooks flags

    (arProg, _) <- requireProgram verbosity arProgram (withPrograms lbi')
    let mkDumpPath l = llvmPrefixTmpDir </> l <.> "dump"

    -- extract the llvm+clang libs we care about into a tmp folder
    notice verbosity "extracting llvm+clang libs.."
    copyFiles verbosity llvmPrefixTmpDir $ map (\l -> (llvmPrefixDir </> "lib", l)) libclangLibrariesFiles
    forM_ libclangLibrariesFiles $ \l -> do
      -- extract each library in its own folder
      let libPath = llvmPrefixDir </> "lib" </> l
          dumpPath = mkDumpPath l
      createDirectoryIfMissingVerbose verbosity True dumpPath
      copyFileVerbose verbosity libPath (dumpPath </> l)
      setCurrentDirectory dumpPath
      extractArLibArchive verbosity arProg l
    setCurrentDirectory curDir

    -- get a list of llvm+clang objects we want to link
    notice verbosity "relinking LibClang library.."
    cObjs <- concat <$> (forM libclangLibrariesFiles $ \l -> do
                             let dumpPath = mkDumpPath l
                             (map (dumpPath </>) . filter isObject) <$> 
                               getDirectoryContents dumpPath)
    -- get the haskell objects
    hsObjs <- getHaskellObjects lib lbi' bdir objExtension (splitObjs lbi')
    -- relink the objs into the package libraries
    let staticObjs = cObjs ++ hsObjs ++ 
                      [bdir </> "src" </> "Clang" </> "Internal" </> mkObject "FFI_stub_ffi"
                      ,bdir </> "cbits" </> mkObject "visitors"]
        libFile = getLibraryName bdir lbi'
    removeFile libFile
    createArLibArchive verbosity arProg libFile staticObjs

libClangCleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
libClangCleanHook pkg v hooks flags = do
  let verbosity = fromFlag (cleanVerbosity flags)
  notice verbosity "Cleaning llvm and clang..."
  curDir <- getCurrentDirectory
  removeDirectoryRecursive $ curDir </> "build"
  (cleanHook simpleUserHooks) pkg v hooks flags
  return ()

libClangInstallHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
libClangInstallHook pkg lbi hooks flags = do
  (instHook simpleUserHooks) pkg lbi hooks flags
  curDir <- getCurrentDirectory
  let llvmLibDir = curDir </> "build" </> "out" </> "lib"
      verbosity = fromFlag (installVerbosity flags)
      libCopyDir = libdir $ absoluteInstallDirs pkg lbi NoCopyDest
  notice verbosity $ "installing clang, llvm shared libs to " ++ libCopyDir
  copyFiles verbosity libCopyDir $ map (\l -> (llvmLibDir, mkSharedLib l)) libclangSharedLibraries

getHaskellObjects :: Library -> LocalBuildInfo -> FilePath -> String -> Bool -> IO [FilePath]
getHaskellObjects lib lbi pref wanted_obj_ext allow_split_objs
  | splitObjs lbi && allow_split_objs = do
        let splitSuffix = if compilerVersion (compiler lbi) <
                             Version [6, 11] []
                          then "_split"
                          else "_" ++ wanted_obj_ext ++ "_split"
            dirs = [ pref </> (ModuleName.toFilePath x ++ splitSuffix)
                   | x <- libModules lib ]
        objss <- mapM getDirectoryContents dirs
        let objs = [ dir </> obj
                   | (objs',dir) <- zip objss dirs, obj <- objs',
                     let obj_ext = takeExtension obj,
                     '.':wanted_obj_ext == obj_ext ]
        return objs
  | otherwise  =
        return [ pref </> ModuleName.toFilePath x <.> wanted_obj_ext
               | x <- libModules lib ]

getLibraryName :: FilePath -> LocalBuildInfo -> FilePath
getLibraryName pref lbi = getLibName $ filter isLib $ componentsConfigs lbi
    where isLib (CLibName, _, _) = True
          isLib _ = False
          getLibName [(_,clbi,_)]
              | LibraryName lname <- componentLibraries clbi !! 0 = pref </> mkStaticLib lname
          getLibName [] = error $ "getLibName: No libraries found!"
          getLibName _ = error $ "getLibName: more than one library found!"

extractArLibArchive :: Verbosity -> ConfiguredProgram -> FilePath -> IO ()
extractArLibArchive verbosity ar target = runProgramInvocation verbosity inv
  where simpleArgs  = ["-x"]
        extraArgs   = verbosityOpts verbosity ++ [target]
        inv  = programInvocation ar (simpleArgs  ++ extraArgs)
        verbosityOpts v | v >= deafening = ["-v"]
                        | otherwise      = []

mkStaticLib :: String -> String
mkStaticLib lname = mkLibName (LibraryName lname)

mkSharedLib :: String -> String
mkSharedLib lname = "lib" ++ lname <.> dllExtension

mkObject :: String -> String
mkObject = (<.> objExtension)

isObject :: String -> Bool
isObject = (== objExtWithDot) . takeExtension
objExtWithDot = "." ++ objExtension
