import Distribution.Simple
import Data.List
import Data.Char (isSpace)
import System.Directory(getCurrentDirectory, removeFile)
import System.IO(IOMode(..), openFile, hPutStr, hFlush, hClose)
import System.IO.Error(catchIOError)
import System.Process(readProcess)
import System.Environment(lookupEnv, getEnv, setEnv)
import System.FilePath.Posix((</>))
import Control.Exception(bracket)

rtrim :: String -> String
rtrim = f . f
  where f = reverse . dropWhile isSpace

getLLVMDetails llvmConfigPath = do
  version <- rtrim <$> readProcess llvmConfigPath ["--version"] []
  cxxFlags <- rtrim <$> readProcess llvmConfigPath ["--cxxflags"] []
  libPath <- rtrim <$> readProcess llvmConfigPath ["--libdir"] []
  includePath <- rtrim <$> readProcess llvmConfigPath ["--includedir"] []
  let cFlags = unwords $ filter (not . isPrefixOf "-std") $ words cxxFlags
  return (version,cFlags,libPath,includePath)

setupLLVMPkgConfig tmpPCPath outf = do
  let tryElse act failAct = catchIOError act (const failAct)
  (version,cFlags,libPath,includePath) <- getLLVMDetails "llvm-config" `tryElse`
                                          getLLVMDetails "llvm-config-3.8" `tryElse`
                                          (getEnv "LLVM_CONFIG" >>= getLLVMDetails) `tryElse`
                                          (error "ERROR: cannot find llvm-config, please setup environment variable LLVM_CONFIG with location of llvm-config and try again")
  let pc = unlines $ ["Name: LLVM"
                     ,"Description: Low-level Virtual Machine compiler framework"
                     ,"Version: " ++ version
                     ,"URL: http://www.llvm.org/"
                     ,"Requires:"
                     ,"Conflicts:"
                     ,"Libs: -L" ++ libPath ++ " -lLLVM-" ++ version
                     ,"Cflags: -I" ++ includePath ++ " " ++ cFlags]
      pkgConfigPath = "PKG_CONFIG_PATH"
  -- putStrLn pc
  hPutStr outf pc
  hFlush outf
  mpc <- lookupEnv pkgConfigPath
  setEnv pkgConfigPath $ case mpc of
    Just pc -> pc ++ ":" ++ tmpPCPath
    Nothing -> tmpPCPath

withTmpPC runM = do
  cwd <- getCurrentDirectory
  let llvmPCFname = cwd </> "llvm.pc"
  bracket
    (openFile llvmPCFname WriteMode)
    -- (\outf -> hClose outf >> removeFile llvmPCFname)
    hClose
    (runM cwd)

main =
  withTmpPC $ \cwd outf -> do
    setupLLVMPkgConfig cwd outf
    defaultMain
