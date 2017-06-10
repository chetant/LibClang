{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.PackageDescription
import Data.Version
import Data.List
import Data.Char(isSpace)
import Data.Maybe
import System.Directory(getCurrentDirectory, removeFile)
import System.IO(IOMode(..), openFile, hPutStr, hFlush, hClose)
import System.IO.Error(catchIOError)
import System.Process(readProcess)
import System.Environment(lookupEnv, getEnv, setEnv)
import System.FilePath.Posix((</>))
import Control.Applicative((<$>))
import Control.Exception(bracket)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.UserHooks
import Distribution.Simple.Program(
                                    getDbProgramOutput,
                                    simpleProgram,
                                    progInvokeInput,
                                    programInvocation,
                                    runProgramInvocation,
                                    arProgram,
                                    lookupProgram
                                  )

import Distribution.Simple.Setup(BuildFlags, ConfigFlags, configConfigurationsFlags, fromFlag, buildVerbosity)
import Distribution.PackageDescription(FlagName)
import Distribution.Verbosity
import Distribution.System(buildOS, OS(OSX))
import Distribution.Simple.Utils(copyFileVerbose, die)
import Distribution.Simple.BuildPaths(mkProfLibName, mkLibName)
import Distribution.Package(ComponentId)
import Control.Monad
import System.FilePath


getLLVMDetails :: IO (String, FilePath, FilePath)
getLLVMDetails =
  let tryElse act failAct = catchIOError act (const failAct)
  in
  getLLVMDetails' "llvm-config-3.8"          `tryElse`
  getLLVMDetails' "llvm-config"              `tryElse`
  (getEnv "LLVM_CONFIG" >>= getLLVMDetails') `tryElse`
  (error "ERROR: cannot find llvm-config, please setup environment variable LLVM_CONFIG with location of llvm-config and try again")
  where
    rtrim :: String -> String
    rtrim = f . f
      where f = reverse . dropWhile isSpace
    getLLVMDetails' :: FilePath -> IO (String, FilePath, FilePath)
    getLLVMDetails' llvmConfigPath = do
      version <- rtrim <$> readProcess llvmConfigPath ["--version"] []
      libPath <- rtrim <$> readProcess llvmConfigPath ["--libdir"] []
      includePath <- rtrim <$> readProcess llvmConfigPath ["--includedir"] []
      return (version,libPath,includePath)

libclangLibraries :: [String]
libclangLibraries =
  [ "clangARCMigrate"
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
  , "clangRewrite"
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

setupLLVMPkgConfig tmpPCPath outf = do
  let tryElse act failAct = catchIOError act (const failAct)
  (version,libPath,includePath) <- getLLVMDetails
  let pc = unlines $ ["Name: LLVM"
                     ,"Description: Low-level Virtual Machine compiler framework"
                     ,"Version: " ++ version
                     ,"URL: http://www.llvm.org/"
                     ,"Requires:"
                     ,"Conflicts:"
                     ,"Libs: -L" ++ libPath ++ " -lLLVM-" ++ version
                     ,"Cflags: -I" ++ includePath]
      pkgConfigPath = "PKG_CONFIG_PATH"
  hPutStr outf pc
  hFlush outf
  mpc <- lookupEnv pkgConfigPath
  let pkgConfigPathVal = case mpc of
        Just pc -> pc ++ ":" ++ tmpPCPath
        Nothing -> tmpPCPath
  setEnv pkgConfigPath pkgConfigPathVal

withTmpPC runM = do
  cwd <- getCurrentDirectory
  let llvmPCFname = cwd </> "llvm.pc"
  bracket
    (openFile llvmPCFname WriteMode)
    (\outf -> hClose outf >> removeFile llvmPCFname)
    (runM cwd)

linkWithLLVMLibs :: LocalBuildInfo -> BuildFlags -> IO ()
linkWithLLVMLibs lbi flags =
  let verbosity = fromFlag (buildVerbosity flags)
      componentLibs = concatMap componentLibNames $ componentsConfigs lbi
   in do
    (_,libPath,_) <- getLLVMDetails
    cwd <- getCurrentDirectory
    -- OS X's linker _really_ wants to link dynamically, and it doesn't support
    -- the options you'd usually use to control that on Linux. We rename the
    -- libclang library to make sure the linker does what we intend.
    -- Merge the libclang static libraries into each Haskell static library.
    let localClangCopy = cwd </> mkStaticLib "clang_static"
    copyFileVerbose verbosity (libPath </> mkStaticLib "clang")
                              localClangCopy
    let libClangLibs = map (libPath </>) (map mkStaticLib libclangLibraries)
        mergeLibs = mergeLibraries verbosity lbi (localClangCopy :  libClangLibs)

    forM_ componentLibs $ \componentLib -> do
      when (withVanillaLib lbi) $
        mergeLibs $ mkLibName componentLib

      when (withProfLib lbi) $
        mergeLibs $ mkProfLibName componentLib

    removeFile localClangCopy
    where
      mkStaticLib :: String -> String
      mkStaticLib lname = "lib" ++ lname <.> "a"

      componentLibNames :: (ComponentName, ComponentLocalBuildInfo, [ComponentName]) -> [UnitId]
      componentLibNames (_, LibComponentLocalBuildInfo {..}, _) = [componentUnitId]
      componentLibNames _                                       = []

      mergeLibraries :: Verbosity -> LocalBuildInfo -> [FilePath] -> FilePath -> IO ()
      mergeLibraries verbosity lbi libclangLibs libName = do
        let libtool = runLibtool verbosity lbi
            ar = runAr verbosity lbi
            bdir = buildDir lbi
            finalLib = bdir </> libName
            origLib = bdir </> libName <.> "orig"
            allLibs = origLib : libclangLibs

        copyFileVerbose verbosity finalLib origLib
        case buildOS of
          OSX -> libtool $ ["-static", "-o", finalLib] ++ allLibs
          _ -> ar ["-M"] $ arScript finalLib allLibs

      arScript :: FilePath -> [FilePath] -> String
      arScript finalLib allLibs = unlines $ ["CREATE " ++ finalLib]
                                         ++ map ("ADDLIB " ++) allLibs
                                         ++ ["SAVE", "END"]

      runAr :: Verbosity -> LocalBuildInfo -> [String] -> String -> IO ()
      runAr v lbi args script =
          case lookupProgram arProgram (withPrograms lbi) of
            Nothing    -> die "Couldn't find required program 'ar'"
            Just cProg -> runProgramInvocation v (progWithStdin cProg)
        where
          progWithStdin prog = (programInvocation prog args') { progInvokeInput = Just script }
          args' = if v >= deafening then "-v" : args else args

      runLibtool :: Verbosity -> LocalBuildInfo -> [String] -> IO ()
      runLibtool verbosity lbi args = do
        output <- getDbProgramOutput verbosity libtoolProgram (withPrograms lbi) args
        when (verbosity >= deafening) $
          putStrLn output

      libtoolProgram = simpleProgram "libtool"

buildHookM :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildHookM pd lbi uh bf = do
  let staticBuildingSpecified = lookup (FlagName "staticbuild") (configConfigurationsFlags (configFlags lbi))
      buildNormally = (buildHook simpleUserHooks) pd lbi uh bf
  maybe
    ((buildHook simpleUserHooks) pd lbi uh bf)
    (\shouldStaticBuild->
       if shouldStaticBuild
       then do
         buildNormally
         linkWithLLVMLibs lbi bf
       else buildNormally)
    staticBuildingSpecified

pcHook = simpleUserHooks { confHook = confHookM, buildHook = buildHookM }
  where confHookM (gpd, hbi) cf = do
          let cdt = fromJust $ condLibrary gpd
              lib = condTreeData cdt
              lbi = libBuildInfo lib
              vRange = withinVersion $ makeVersion [3,8]
              lbi' = lbi { pkgconfigDepends = pkgconfigDepends lbi ++
                                              [Dependency (PackageName "llvm") vRange] }
              lib' = lib { libBuildInfo = lbi' }
              gpd' = gpd { condLibrary = Just (cdt { condTreeData = lib' }) }
          (confHook simpleUserHooks) (gpd', hbi) cf

#if __GLASGOW_HASKELL__ < 710
makeVersion xs = Version xs []
#endif

main =
  withTmpPC $ \cwd outf -> do
    setupLLVMPkgConfig cwd outf
    defaultMainWithHooks pcHook
