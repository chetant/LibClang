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

rtrim :: String -> String
rtrim = f . f
  where f = reverse . dropWhile isSpace

getLLVMDetails llvmConfigPath = do
  version <- rtrim <$> readProcess llvmConfigPath ["--version"] []
  libPath <- rtrim <$> readProcess llvmConfigPath ["--libdir"] []
  includePath <- rtrim <$> readProcess llvmConfigPath ["--includedir"] []
  return (version,libPath,includePath)

setupLLVMPkgConfig tmpPCPath outf = do
  let tryElse act failAct = catchIOError act (const failAct)
  (version,libPath,includePath) <- getLLVMDetails "llvm-config-3.8" `tryElse`
                                   getLLVMDetails "llvm-config" `tryElse`
                                   (getEnv "LLVM_CONFIG" >>= getLLVMDetails) `tryElse`
                                   (error "ERROR: cannot find llvm-config, please setup environment variable LLVM_CONFIG with location of llvm-config and try again")
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
  -- putStrLn $ pkgConfigPath ++ "=" ++ pkgConfigPathVal
  setEnv pkgConfigPath pkgConfigPathVal

withTmpPC runM = do
  cwd <- getCurrentDirectory
  let llvmPCFname = cwd </> "llvm.pc"
  bracket
    (openFile llvmPCFname WriteMode)
    (\outf -> hClose outf >> removeFile llvmPCFname)
    (runM cwd)

pcHook = simpleUserHooks { confHook = confHookM }
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
