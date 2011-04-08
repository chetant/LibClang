import System(getArgs)
import Data.Maybe(fromJust, isJust)
-- import qualified Clang.TranslationUnit as Clang
-- import qualified Clang.Diagnostic as Diagnostic
import qualified Clang.FFI as Clang

test args = do
  -- args <- getArgs
  index <- Clang.createIndex False False
  mtu <- Clang.parse index Nothing args [] [Clang.TranslationUnit_None]
  let tu = if isJust mtu then fromJust mtu else error "No TXUnit!"
      diags = Diagnostic.getDiagnostics tu
  print diags

  -- let printDiag i = do
  --        diag <- Clang.getDiagnostic tu i
  --        clstr <- Clang.formatDiagnostic diag Diagnostic.getDefaultDisplayOptions
  --        str <- Clang.getCString clstr
  --        putStrLn str
  --        Clang.disposeString clstr
  -- numDiags <- Clang.getNumDiagnostics tu
  -- putStrLn $ "numDiags:" ++ show numDiags
  -- mapM_ printDiag [0..numDiags]
  -- Clang.disposeTranslationUnit tu
  -- Clang.disposeIndex index
  -- putStrLn "hehe"

main = test ["-I/home/saiko/bin/llvm/include", "testclang.c"]